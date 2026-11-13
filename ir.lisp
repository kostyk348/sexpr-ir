;;; ir.lisp — исполняемый S-expression IR для проверки переimplementаций
;;;
;;; Идея: логика функции описывается S-expression, который читается самим
;;; ридером Lisp (гомоиконичность — AST и есть данные), интерпретируется на
;;; корпусе моков за микросекунды и только потом печатается в C.
;;;
;;; Запуск:  SBCL_HOME=/usr/local/sbcl/lib sbcl --script ir.lisp
;;; REPL:    SBCL_HOME=/usr/local/sbcl/lib sbcl --load ir.lisp \
;;;            --eval '(sexpr-repl)'

(defvar *mocks* (make-hash-table :test #'equal)
  "Моки системных вызовов: (имя . аргументы) -> результат.")

(defvar *memory* (make-hash-table :test #'equal)
  "Моки объектов: имя -> plist полей.")

(defvar *corpus* (make-hash-table :test #'equal)
  "Тестовый корпус: имя функции -> список (аргументы ожидание).")

(defvar *functions* (make-hash-table :test #'equal)
  "Зарегистрированные IR-функции: имя -> AST.")

(defun mock-syscall (name args result)
  "Зарегистрировать мок системного вызова NAME с аргументами ARGS."
  (setf (gethash (cons name args) *mocks*) result))

(defun mock-object (name plist)
  "Зарегистрировать мок объекта: NAME — символ, PLIST — поля."
  (setf (gethash name *memory*) plist))

;;; ------------------------------------------------------------------
;;; 1. Чтение: S-expression разбирает ридер Lisp, а не свой парсер
;;; ------------------------------------------------------------------

(defun read-ir (text)
  "Прочитать один S-expression из TEXT."
  (multiple-value-bind (form pos)
      (read-from-string text nil :eof)
    (when (eq form :eof)
      (error "Пустой S-expression"))
    (values form pos)))

;;; ------------------------------------------------------------------
;;; 2. Части IR-формы
;;; ------------------------------------------------------------------

(defun ir-function-p (form)
  (and (consp form) (eq (first form) :function)))

(defun ir-name (form)
  (second form))

(defun ir-params (form)
  (second (assoc :params (cddr form))))

(defun ir-body (form)
  (second (assoc :body (cddr form))))

(defun define-ir (form)
  "Зарегистрировать IR-функцию FORM, вернуть её имя."
  (unless (ir-function-p form)
    (error "Не :function: ~a" form))
  (let ((name (ir-name form)))
    (setf (gethash name *functions*) form)
    name))

;;; ------------------------------------------------------------------
;;; 3. Интерпретатор
;;; ------------------------------------------------------------------

(defun ir-field (obj field)
  "Прочитать поле FIELD у мок-объекта OBJ."
  (let ((plist (gethash obj *memory*)))
    (unless plist
      (error "Нет мока объекта: ~a" obj))
    (getf plist field)))

(defun ir-eval (form env)
  "Вычислить IR-форму FORM в окружении ENV — списке (имя . значение)."
  (cond
    ((numberp form) form)
    ((stringp form) form)
    ((symbolp form)
     (let ((cell (assoc form env)))
       (if cell
           (cdr cell)
           (error "Неизвестная переменная: ~a" form))))
    ((consp form)
     (let ((op (first form))
           (args (rest form)))
       (case op
         (:if
          (if (ir-eval (first args) env)
              (ir-eval (second args) env)
              (ir-eval (third args) env)))
         (:null
          (null (ir-eval (first args) env)))
         (:not
          (not (ir-eval (first args) env)))
         (:eq
          (equal (ir-eval (first args) env)
                 (ir-eval (second args) env)))
         (:>=
          (>= (ir-eval (first args) env)
              (ir-eval (second args) env)))
         (:<=
          (<= (ir-eval (first args) env)
              (ir-eval (second args) env)))
         (:>
          (> (ir-eval (first args) env)
             (ir-eval (second args) env)))
         (:<
          (< (ir-eval (first args) env)
             (ir-eval (second args) env)))
         (:and
          (every (lambda (a) (ir-eval a env)) args))
         (:or
          (some (lambda (a) (ir-eval a env)) args))
         (:+
          (+ (ir-eval (first args) env)
             (ir-eval (second args) env)))
         (:-
          (- (ir-eval (first args) env)
             (ir-eval (second args) env)))
         (:*
          (* (ir-eval (first args) env)
             (ir-eval (second args) env)))
         (:field
          (ir-field (ir-eval (first args) env) (second args)))
         (:mem/read
          (if (and (consp (first args)) (eq (first (first args)) :field))
              (ir-field (ir-eval (second (first args)) env)
                        (third (first args)))
              0))
         (:syscall
          (let ((name (first args))
                (vals (mapcar (lambda (a) (ir-eval a env)) (rest args))))
            (multiple-value-bind (value found)
                (gethash (cons name vals) *mocks*)
              (unless found
                (error "Не замокан вызов: ~a ~a" name vals))
              value)))
         (t (error "Неизвестная операция: ~a" op)))))
    (t (error "Не IR-форма: ~a" form))))

(defun ir-call (name args)
  "Вызвать IR-функцию NAME с аргументами ARGS."
  (let* ((fn (gethash name *functions*))
         (params (ir-params fn)))
    (unless fn
      (error "Функция ~a не зарегистрирована" name))
    (ir-eval (ir-body fn) (pairlis params args))))

;;; ------------------------------------------------------------------
;;; 4. Генератор C
;;; ------------------------------------------------------------------

(defun c-symbol (sym)
  "Имя символа в стиле C."
  (string-downcase (symbol-name sym)))

(defun c-expr (form)
  "Напечатать IR-форму как C-выражение."
  (cond
    ((numberp form) (format nil "~d" form))
    ((stringp form) (format nil "\"~a\"" form))
    ((symbolp form) (c-symbol form))
    ((consp form)
     (let ((op (first form))
           (args (rest form)))
       (case op
         (:if (format nil "((~a) ? ~a : ~a)"
                      (c-expr (first args))
                      (c-expr (second args))
                      (c-expr (third args))))
         (:null (format nil "(~a == NULL)" (c-expr (first args))))
         (:not (format nil "(!~a)" (c-expr (first args))))
         (:eq (format nil "(~a == ~a)"
                      (c-expr (first args))
                      (c-expr (second args))))
         (:>= (format nil "(~a >= ~a)"
                      (c-expr (first args))
                      (c-expr (second args))))
         (:<= (format nil "(~a <= ~a)"
                      (c-expr (first args))
                      (c-expr (second args))))
         (:> (format nil "(~a > ~a)"
                     (c-expr (first args))
                     (c-expr (second args))))
         (:< (format nil "(~a < ~a)"
                     (c-expr (first args))
                     (c-expr (second args))))
         (:and (format nil "(~{~a~^ && ~})" (mapcar #'c-expr args)))
         (:or (format nil "(~{~a~^ || ~})" (mapcar #'c-expr args)))
         (:+ (format nil "(~a + ~a)"
                     (c-expr (first args))
                     (c-expr (second args))))
         (:- (format nil "(~a - ~a)"
                     (c-expr (first args))
                     (c-expr (second args))))
         (:*
          (format nil "(~a * ~a)"
                  (c-expr (first args))
                  (c-expr (second args))))
         (:field
          (format nil "~a->~a" (c-expr (first args))
                  (c-symbol (second args))))
         (:mem/read
          (if (and (consp (first args)) (eq (first (first args)) :field))
              (c-expr (first args))
              "0"))
         (:syscall
          (format nil "~a(~{~a~^, ~})"
                  (c-symbol (first args))
                  (mapcar #'c-expr (rest args))))
         (t (format nil "/* не поддержано: ~a */ 0" op)))))
    (t "0")))

(defun ir->c (form)
  "Сгенерировать C-функцию из IR-формы."
  (format nil "int ~a(~{~a~^, ~}) {~%    return ~a;~%}~%"
          (c-symbol (ir-name form))
          (mapcar (lambda (p) (format nil "void *~a" (c-symbol p)))
                  (ir-params form))
          (c-expr (ir-body form))))

;;; ------------------------------------------------------------------
;;; 5. Корпус и прогон
;;; ------------------------------------------------------------------

(defun add-cases (name cases)
  "Добавить случаи: NAME — функция, CASES — список (аргументы ожидание)."
  (setf (gethash name *corpus*) cases))

(defun run-corpus (name &key (stream *standard-output*))
  "Прогнать корпус для NAME. Вернуть число провалов."
  (let ((cases (gethash name *corpus*))
        (fails 0))
    (unless cases
      (error "Нет корпуса для ~a" name))
    (format stream "~&~%=== ~a: ~d случая ===~%" name (length cases))
    (loop for case in cases
          for i from 1
          for args = (first case)
          for expected = (second case)
          for got = (ir-call name args)
          do (if (equal got expected)
                 (format stream "~&  ~2d. ~a => ~a  ok~%" i args got)
                 (progn
                   (incf fails)
                   (format stream "~&  ~2d. ~a => ~a  FAIL, ждали ~a~%"
                           i args got expected))))
    (format stream "~&  итог: ~d ok, ~d fail~%"
            (- (length cases) fails) fails)
    fails))

;;; ------------------------------------------------------------------
;;; 6. Замер скорости
;;; ------------------------------------------------------------------

(defun time-iterations (thunk iterations)
  "Секунды на ITERATIONS вызовов THUNK."
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (declare (ignore i))
      (funcall thunk))
    (/ (- (get-internal-real-time) start)
       (float internal-time-units-per-second))))

(defun bench (name &key (iterations 100000))
  "Замерить среднее время одного интерпретированного прогона."
  (let* ((args (first (first (gethash name *corpus*))))
         (seconds (time-iterations (lambda () (ir-call name args))
                                   iterations)))
    (format t "~&~a: ~d прогонов за ~,3f с = ~,2f мкс/прогон~%"
            name iterations seconds
            (* 1000000.0 (/ seconds iterations)))
    seconds))

;;; ------------------------------------------------------------------
;;; 7. Демо
;;; ------------------------------------------------------------------

(defun setup-demo ()
  "Заполнить моки, IR-функции и корпус."
  (clrhash *mocks*)
  (clrhash *memory*)
  (clrhash *functions*)
  (clrhash *corpus*)

  (mock-object 'str-hello '(:length 5 :characters "hello"))
  (mock-object 'str-empty '(:length 0 :characters ""))
  (mock-syscall 'cfstring_len '("hello") 5)

  ;; 1) Пример из постановки: геттер длины
  (define-ir
   (read-ir "(:function cfstring_get_length
              (:params (str))
              (:body
                (:if (:null str) 0
                     (:mem/read (:field str :length)))))"))
  (add-cases 'cfstring_get_length
             '(((nil) 0) ((str-hello) 5) ((str-empty) 0)))

  ;; 2) Ветвление и арифметика
  (define-ir
   (read-ir "(:function cfstring_clamped_length
              (:params (str))
              (:body
                (:if (:null str)
                      0
                      (:if (:eq (:field str :length) 0)
                           0
                           (:+ (:field str :length) 1)))))"))
  (add-cases 'cfstring_clamped_length
             '(((nil) 0) ((str-empty) 0) ((str-hello) 6)))

  ;; 3) Вызов системного вызова
  (define-ir
   (read-ir "(:function cfstring_length_via_syscall
              (:params (chars))
              (:body
                (:if (:null chars) 0
                     (:syscall cfstring_len chars))))"))
  (add-cases 'cfstring_length_via_syscall
             '(((nil) 0) (("hello") 5)))
  t)

;;; ------------------------------------------------------------------
;;; 7b. Реальные функции VibeDarling
;;;
;;; Провенанс: github.com/VibeDarling/darling-foundation
;;; коммит 36f348ca7392a26034ab6332db7d2ed32da5182c.
;;; Моделируется только решающая логика; объектно-системная обвязка
;;; (NSException, NSString) остаётся в ObjC и здесь заменена моком.
;;; ------------------------------------------------------------------

(defvar *vibedarling-functions*
  '(objc_constant_array_object_at_index
    objc_bytecount_thousand
    objc_bytecount_fraction_digits)
  "Функции, смоделированные из кода VibeDarling.")

(defun setup-vibedarling ()
  "Зарегистрировать IR-модели реальных функций VibeDarling."
  ;; ModernStubs.m:16-25 — -[NSConstantArray objectAtIndex:]
  ;;
  ;;   - (id)objectAtIndex:(NSUInteger)index
  ;;   {
  ;;       if (index >= _count) {
  ;;           [NSException raise:NSRangeException ...];
  ;;           return nil;
  ;;       }
  ;;       return (id)_objects[index];
  ;;   }
  ;;
  ;; Модель возвращает -1 вместо исключения и самого индекса вместо
  ;; объекта: важно проверить именно граничное условие >=.
  (define-ir
   (read-ir "(:function objc_constant_array_object_at_index
              (:params (index count))
              (:body
                (:if (:>= index count) -1 index)))"))
  (add-cases 'objc_constant_array_object_at_index
             '(((0 3) 0)
               ((2 3) 2)
               ((3 3) -1)
               ((4 3) -1)
               ((0 0) -1)))

  ;; NSByteCountFormatter.m:125-134 — выбор основания
  ;;
  ;;   unsigned long long thousand = 0;
  ;;   switch (_countStyle)
  ;;   {
  ;;       case NSByteCountFormatterCountStyleBinary:
  ;;       case NSByteCountFormatterCountStyleMemory:
  ;;           thousand = 1024;
  ;;           break;
  ;;       default:
  ;;           thousand = 1000;
  ;;           break;
  ;;   }
  ;;
  ;; style: 0 = binary, 1 = memory, 2 = decimal.
  (define-ir
   (read-ir "(:function objc_bytecount_thousand
              (:params (style))
              (:body
                (:if (:or (:eq style 0) (:eq style 1)) 1024 1000)))"))
  (add-cases 'objc_bytecount_thousand
             '(((0) 1024) ((1) 1024) ((2) 1000) ((3) 1000)))

  ;; NSByteCountFormatter.m:182-195 — adaptive fraction digits
  ;;
  ;;   NSUInteger fractionDigits = 2;
  ;;   if (_adaptive) {
  ;;       if (unitIdx <= 1) { fractionDigits = 0; }
  ;;       else if (unitIdx == 2) { fractionDigits = 1; }
  ;;   }
  ;;
  ;; В C `_adaptive` — целое, и 0 означает «ложь», а в Lisp 0 — истина.
  ;; Поэтому условие записано явно: (:not (:eq adaptive 0)).
  ;; Это первый настоящий баг семантики, который IR поймал до компиляции.
  (define-ir
   (read-ir "(:function objc_bytecount_fraction_digits
              (:params (unit-idx adaptive))
              (:body
                (:if (:not (:eq adaptive 0))
                     (:if (:<= unit-idx 1)
                          0
                          (:if (:eq unit-idx 2) 1 2))
                     2)))"))
  (add-cases 'objc_bytecount_fraction_digits
             '(((0 1) 0)
               ((1 1) 0)
               ((2 1) 1)
               ((3 1) 2)
               ((0 0) 2)
               ((7 0) 2)))
  t)

(defun demo ()
  "Полный прогон: чтение, интерпретация, C, замер."
  (setup-demo)
  (setup-vibedarling)
  (format t "~%S-expression IR prototype~%")

  (format t "~%-- 1. Форму читает ридер Lisp --~%")
  (multiple-value-bind (form pos)
      (read-ir "(:function f (:params (x)) (:body (:+ x 1)))")
    (format t "~&форма: ~s~%позиция: ~d~%" form pos))

  (format t "~%-- 2. Интерпретация на корпусе --~%")
  (let ((fails 0))
    (maphash (lambda (name form)
               (declare (ignore form))
               (incf fails (run-corpus name)))
             *functions*)

    (format t "~%-- 3. Сгенерированный C --~%")
    (maphash (lambda (name form)
               (format t "~%~a" (ir->c form)))
             *functions*)

    (format t "~%-- 4. Скорость --~%")
    (bench 'cfstring_get_length :iterations 100000)

    (format t "~%-- итог --~%")
    (if (zerop fails)
        (format t "~&все случаи прошли~%")
        (format t "~&провалов: ~d~%" fails))
    (zerop fails)))

;;; ------------------------------------------------------------------
;;; 8. REPL
;;; ------------------------------------------------------------------

(defun sexpr-repl ()
  "REPL: регистрирует IR-функции, печатает C, гоняет корпус."
  (setup-demo)
  (format t "~%S-expression IR REPL~%")
  (format t "  (:function имя (:params (a b)) (:body ...)) — регистрация~%")
  (format t "  (c <форма>) — показать C~%")
  (format t "  (run <имя>) — прогнать корпус~%")
  (format t "  (demo) — демо~%")
  (format t "  exit — выход~%~%")
  (loop
    (format t "ir> ")
    (finish-output)
    (let ((line (read-line nil nil)))
      (when (null line) (return))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((string= trimmed "") nil)
          ((string= trimmed "exit") (return))
          ((string= trimmed "demo") (demo))
          (t
           (handler-case
               (multiple-value-bind (form pos) (read-ir trimmed)
                 (declare (ignore pos))
                 (cond
                   ((ir-function-p form)
                    (format t "~&зарегистрировано: ~a~%"
                            (define-ir form)))
                   ((and (consp form) (eq (first form) 'c))
                    (format t "~%~a" (ir->c (second form))))
                   ((and (consp form) (eq (first form) 'run))
                    (run-corpus (second form)))
                   (t (format t "~&форма: ~s~%" form))))
             (error (e)
               (format t "~&ошибка: ~a~%" e)))))))))

;;; ------------------------------------------------------------------

(unless (find :keep sb-ext:*posix-argv* :test #'string=)
  (sb-ext:exit :code (if (demo) 0 1)))
