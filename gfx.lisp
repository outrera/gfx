(defpackage :gfx (:use :cl)
  (:export gfx gfx-new gfx-w gfx-h gfx-c gfx-d gfx-m gfx-k gfx-k gfx-info gfx-p gfx-hs-x gfx-hs-y
           gfx-get gfx-set rgb w/rgb colors-to-bytes bytes-to-colors colors-to-colors
           gfx-blit  gfx-chn gfx-margins gfx-avg gfx-circle gfx-rect gfx-line gfx-clear
           gfx-load-error gfx-set-handler gfx-load gfx-make
           ))
(in-package :gfx)


(defmacro with-gensyms (names &body body)
  `(let ,(loop for name in names collect `(,name (gensym ,(string name))))
     ,@body))

(defun displace (start size array)
  (make-array size
              :element-type (array-element-type array)
              :displaced-to array :displaced-index-offset start))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rng (a b)
    (if (< a b)
        (loop as i from a to b collect i)
        (loop as i from a downto b collect i))))

(defun load-file (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun save-file (path bytes)
  (with-open-file (stream path
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes stream))
  nil)

(defun atake (n xs)
  (if (>= n (length xs))
      xs
      (multiple-value-bind (a start) (array-displacement xs)
        (if a
            (displace start n a)
            (displace 0 n xs)))))

(defun adrop (n xs)
  (when (< n (length xs))
    (multiple-value-bind (a start) (array-displacement xs)
      (if a
          (displace (+ start n) (- (length xs) n) a)
          (displace n (- (length xs) n) xs)))))

(defun acmp (xs ys)
  (let ((l (min (length xs) (length ys))))
    (dotimes (i l)
      (when (/= (aref xs i) (aref ys i))
        (return-from acmp nil)))
    t))

(defun acpy (d s)
  (let ((l (min (length d) (length s))))
    (dotimes (i l)
      (setf (aref d i) (aref s i))))
  d)

(defun acat (a b)
  (concatenate `(simple-array ,(array-element-type a) (*)) a b))

(deftype u1 () '(unsigned-byte 8))
(deftype u2 () '(unsigned-byte 16))
(deftype u4 () '(unsigned-byte 32))
(deftype u8 () '(unsigned-byte 64))
(deftype s1 () '(signed-byte 8))
(deftype s2 () '(signed-byte 16))
(deftype s4 () '(signed-byte 32))
(deftype s8 () '(signed-byte 64))
(deftype cptr () 'sb-sys:system-area-pointer)
;(deftype cptr () '(sb-alien:alien (* t)))

;; fetch bytes
(defmacro fb (n x) `(ldb (byte 8 ,(* n 8)) ,x))

;; create a word from bytes
(defmacro cw (&rest xs)
  `(the u4 (logior ,@(mapcar (lambda (x n) `(ash (the (integer 0 255) ,x) ,(* 8 n)))
                             xs (rng 0 (- (length xs) 1))))))
 

(defmacro vec (size type &rest dup)
  `(make-array ,size :element-type ',type
                     :initial-element ,(if dup (car dup) 0)))

(defun string-to-utf8 (string) (sb-ext:string-to-octets string))
(defun utf8-to-string (bytes)
  (sb-ext:octets-to-string
   (coerce bytes '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)))))



;; unsigned mod32 arithmetic
(declaim (inline u+) (ftype (function (u4 u4) u4) u+))
(defun u+ (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (+ a b)))
#+sbcl (define-compiler-macro u+ (a b) `(ldb (byte 32 0) (+ ,a ,b)))

(declaim (inline u-) (ftype (function (u4 u4) u4) u-))
(defun u- (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (- a b)))
#+sbcl (define-compiler-macro u- (a b) `(ldb (byte 32 0) (- ,a ,b)))

(declaim (inline u*) (ftype (function (u4 u4) u4) u*))
(defun u* (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (* a b)))
#+sbcl (define-compiler-macro u* (a b) `(ldb (byte 32 0) (* ,a ,b)))

(declaim (inline u/) (ftype (function (u4 u4) u4) u/))
(defun u/ (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (truncate a b)))
#+sbcl (define-compiler-macro u/ (a b) `(ldb (byte 32 0) (truncate ,a ,b)))

(declaim (inline umod) (ftype (function (u4 u4) u4) umod))
(defun umod (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (mod a b)))
#+sbcl (define-compiler-macro umod (a b) `(ldb (byte 32 0) (mod ,a ,b)))

(declaim (inline uxor) (ftype (function (u4 u4) u4) uxor))
(defun uxor (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (logxor a b)))
#+sbcl (define-compiler-macro uxor (a b) `(ldb (byte 32 0) (logxor ,a ,b)))

(declaim (inline uand) (ftype (function (u4 u4) u4) uand))
(defun uand (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (logand a b)))
#+sbcl (define-compiler-macro uand (a b) `(ldb (byte 32 0) (logand ,a ,b)))

(declaim (inline uior) (ftype (function (u4 u4) u4) uior))
(defun uior (a b)
  (declare (type u4 a b))
  (ldb (byte 32 0) (logior a b)))
#+sbcl (define-compiler-macro uior (a b) `(ldb (byte 32 0) (logior ,a ,b)))

(declaim (inline u<<) (ftype (function (u4 (unsigned-byte 5)) u4) u<<))
(defun u<< (num count)
  (declare (type u4 num))
  (declare (type (unsigned-byte 5) count))
  (ldb (byte 32 0) (ash num count)))
#+sbcl (define-compiler-macro u<< (num count)
         `(logand #xffffffff (ash (the u4 ,num) (the (unsigned-byte 5) ,count))))


(declaim (inline u>>) (ftype (function (u4 (unsigned-byte 5)) u4) u>>))
(defun u>> (num count)
  (declare (type u4 num))
  (declare (type (unsigned-byte 5) count))
  (ldb (byte 32 0) (ash num (the (signed-byte 6) (- count)))))
#+sbcl (define-compiler-macro u>> (num count)
         `(logand #xffffffff (ash (the u4 ,num) (the (signed-byte 6) (- (the (unsigned-byte 5) ,count))))))


(declaim (inline unot) (ftype (function (u4) u4) unot))
(defun unot (x)
  (declare (type u4 x))
  (ldb (byte 32 0) (lognot x)))
#+sbcl (define-compiler-macro unot (num) `(ldb (byte 32 0) (lognot ,num)))

(declaim (inline uneg) (ftype (function (u4) u4) uneg))
(defun uneg (x)
  (declare (type u4 x))
  (ldb (byte 32 0) (- x)))
#+sbcl (define-compiler-macro uneg (num) `(ldb (byte 32 0) (lognot ,num)))


(defmacro f+ (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (+ (the fixnum ,x) (the fixnum ,y))))))

(defmacro f- (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (- (the fixnum ,x) (the fixnum ,y))))))

(defmacro f* (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (* (the fixnum ,x) (the fixnum ,y))))))

(defmacro f/ (a b)
  (with-gensyms (x y)
    `(let ((,x ,a)
           (,y ,b))
       (the fixnum (truncate (the fixnum ,x) (the fixnum ,y))))))


(defmacro f>> (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (ash (the fixnum ,x) (the fixnum (- ,y)))))))


(defmacro f<< (a b)
  (with-gensyms (x y)
    `(let ((,x ,a) (,y ,b))
       (the fixnum (ash (the fixnum ,x) (the fixnum ,y))))))


(defun %grow (s l)
  (declare ((SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) s)
           (fixnum l))
  (let* ((n (length s))
         (d (vec (* 2 (+ n l)) u1)))
    (dotimes (i n) (setf (aref d i) (aref s i)))
    d))

(defmacro %new (&rest as) `(cons (vec ,(if as (car as) 0) u1) 0))
(defmacro %ptr (x) `(cons (the (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) ,x) 0))
(defun %clone (x) (cons (car x) (cdr x)))
(defun %crop (x) 
  (let* ((d (vec (cdr x) u1))
         (s (car x)))
    (dotimes (i (length d))
      (setf (aref d i) (aref s i)))
    d))
(defmacro %arr (x) `(the (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) (car ,x)))
(defmacro %off (x) `(the fixnum (cdr ,x)))

(defun %put (p s)
  (let* ((d (%arr p))
         (i (%off p))
         (n (+ i (length s))))
    (when (< (length d) n)
      (let ((nd (vec (* 2 n) u1)))
        (acpy nd d)
        (setf d nd)))
    (loop as v across s
       do (progn (setf (aref d i) v)
                 (incf i)))
    (setf (car p) d
          (cdr p) i)
    p))

(defun %get (p n)
  (let* ((s (%arr p))
         (j (%off p))
         (l (- (length s) j))
         (d (vec (min n l) u1)))
    (dotimes (i (length d))
      (setf (aref d i) (aref s j))
      (incf j))
    (setf (cdr p) j)
    d))


(defmacro % (&rest as)
  "Usage: % pointer [offset] [value-to-store]"
  (let ((n 1)
        (p nil)
        (v (gensym))
        (g (gensym))
        (a (gensym))
        (i (gensym)))
    (when (numberp (first as))
      (setf n  (car as)
            as (cdr as)))
    (setf p  (car as)
          as (cdr as))
    `(let* ((,g ,p)
            (,a (%arr ,g))
            ,@(when as `((,v ,(car as))))
            (,i (the fixnum (cdr ,g))))
       ,@(when as
           `((when (<= (length ,a) ,i)
               (setf ,a (the (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) (%grow ,a 1)))
               (setf (car ,g) ,a))))
       (prog1 (aref ,a ,i)
         ,@(when as `((setf (aref ,a ,i) (logand #xFF ,v))))
         ,@(unless (eql n 0) `((setf (cdr ,g) (the fixnum (+ ,i ,n)))))))))


(defmacro raw  (n s f)
  (with-gensyms (x)
    `(let ((,x (%get ,s ,n)))
       (if (= (length ,x) ,n)
           ,x
           ,f))))

(defun lsbmsb (n s f &key lsb)
  (with-gensyms (tmp)
    `(let ((,tmp (raw ,n ,s ,f)))
       (logior ,@(mapcar (lambda (i) `(ash (aref ,tmp ,i)
                                           ,(* (if lsb i (- n 1 i)) 8)))
                         (rng 0 (- n 1)))))))

(defmacro lsb  (n s f) (lsbmsb n s f :lsb t))
(defmacro msb  (n s f) (lsbmsb n s f :lsb nil))
(defmacro utf8 (n s f) `(utf8-to-string (raw ,n ,s ,f)))


(defmacro deser (stream fields &body body)
  (let* ((s (gensym))
         (b (gensym))
         (l (gensym))
         (f `(error "failed to deserialize stream")))
    (when (eql (first body) :on-fail)
      (setf f (second body))
      (setf body (cddr body)))
    (let ((bs (mapcar (lambda (x)
                        (when (numberp (second x))
                          (setf x (cons (car x) (cons 'lsb (cdr x)))))
                        (let* ((h (car x))
                               (b `(,@(cdr x) ,s (funcall ,l)))
                               (g (gensym))
                               (c (gensym)))
                          (when (consp h)
                            (setf b `(let ((,g ,b)
                                           (,c ,(second h)))
                                       (unless (if (arrayp ,c)
                                                   (acmp ,g ,c)
                                                   (equal ,g ,c))
                                         ,f)))
                            (setf h g))
                          `(,h ,b)))
                      fields)))
      `(block ,b
         (let* ((,l (lambda () ,f))
                (,s ,stream)
                (,s (if (arrayp ,s) (%ptr ,s) ,s)) ;in case user passes raw array
                ,@bs)
           (declare (ignorable ,@(mapcar #'first bs)))
           ,@body)))))


(defmacro ser-arr  (s &rest as)
  (let* ((ys (car (last as)))
         (as (butlast as))
         (y (gensym)))
    (if (and (= (length as) 1) (eq (car as) 1))
        `(%put ,s ,ys)
        (progn (when (numberp (car as))
                 (setf as (cons 'lsb as)))
               `(loop as ,y across ,ys
                   do (,(intern (concatenate 'string "SER-" (symbol-name (car as))))
                        ,s
                        ,@(cdr as)
                        ,y))))))

(defmacro ser-dup (s n &rest x)
  (when (numberp (car x))
    (setf x (cons 'lsb x)))
  `(loop as i from 0 below ,n
      do (,(intern (concatenate 'string "SER-" (symbol-name (car x))))
           ,s
           ,@(cdr x))))


(defun ser-lsbmsb (stream n val &key lsb)
  (with-gensyms (s v)
    `(let ((,s ,stream)
           (,v ,val))
       ,@(mapcar (lambda (i) `(% ,s (ash ,v ,(- (* (if lsb i (- n 1 i)) 8)))))
                 (rng 0 (- n 1))))))

(defmacro ser-lsb  (s n v) (ser-lsbmsb s n v :lsb t))
(defmacro ser-msb  (s n v) (ser-lsbmsb s n v :lsb nil))
(defmacro ser-utf8 (s v) `(ser-arr ,s 1 (string-to-utf8 ,v)))


(defmacro ser (stream &body fields)
  (with-gensyms (s)
    `(let* ((,s ,(if (eq stream t) `(%new) stream)))
       ,@(mapcar (lambda (x)
                   (let* (;(n (car x))
                          (x (cdr x)))
                     (when (numberp (car x))
                       (setf x (cons 'lsb x)))
                     `(,(intern (concatenate 'string "SER-" (symbol-name (car x))))
                        ,s
                        ,@(cdr x))))
                 fields)
       ,@(when (eq stream t) `((%crop ,s)))
       )))

;; sequence coerce
(defmacro sc (elem-type seq) `(coerce ,seq '(simple-array ,elem-type (*))))

(defmacro u4-u1-m (n x msb)
  (with-gensyms (o i c p)
    `(let* ((,p ,x)
            (,o (vec (* ,n (length ,p)) u1))
            (,i 0))
       (times ,c (length ,p)
         ,@(loop as j below n collect
                `(progn (setf (aref ,o ,i) (ldb (byte 8 ,(if msb (* 8 (- n 1 j)) (* 8 j)))
                                                (aref ,p ,c)))
                        (incf ,i))))
       ,o)))

(defmacro collect (v s e &body body)
  `(mapcar (lambda (,v) ,@body) (rng ,s ,e)))

(defmacro while (expr &body body)
  (with-gensyms (var)
    `(do ((,var ,expr ,expr))
         ((not ,var))
       ,@body)))

(defmacro times (v n &body body)
  (with-gensyms (c)
    `(do ((,v 0 (1+ ,v))
          (,c ,n))
         ((>= ,v ,c) NIL)
       (declare (type fixnum ,v ,c)
                (ignorable ,v))
       ,@body)))

(defmacro bind-struct (type struct fs &body b)
  (let ((s (gensym)))
    `(let ((,s ,struct))
       (symbol-macrolet
           ,(mapcar (lambda (x)
                      (let* ((v (if (listp x) (first x) x))
                             (x (if (listp x) (second x) x))
                              (a (intern (concatenate 'string (symbol-name type) "-" (symbol-name x))
                                         (symbol-package type))))
                        `(,v (,a ,s))))
                    fs)
       ,@b))))




;; breaks array `s` of u4 into `c` interleaved channels of `w` bytes each
(defun u4-u1 (c s &key (w 1) (msb t))
  (declare (optimize (safety 0) (speed 3))
           (fixnum c w)
           (type (SIMPLE-ARRAY u4 (*)) s))
  (when (and (= w 1) (< c 5))
    (return-from u4-u1
      (if msb
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u4-u1-m ,i s t))))
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u4-u1-m ,i s nil)))))))
  (let* ((d (vec (f* c (length s)) u1))
         (h (f/ (length s) w))
         (i 0)
         (j 0)
         (wc (f* w c)))
    (declare (fixnum wc i j))
    (macrolet ((m (msb)
                 `(times y h
                    (times x w
                      (let ((v (aref s i)))
                        (times k c
                          (setf (aref d j) (ldb (byte 8 ,(if msb
                                                             `(f* 8 (f- (f- c 1) k))
                                                             `(f* 8 k)))
                                                v))
                          (incf j w)))
                      (incf i)
                      (decf j wc)
                      (incf j))
                    (incf j (f* w (f- c 1))))))
      (if msb (m t) (m nil)))
    d))

(defmacro u1-u4-m (n x msb)
  (with-gensyms (o i c p)
    `(let* ((,p ,x)
            (,o (vec (truncate (length ,p) ,n) u4))
            (,c 0))
       (declare (fixnum ,c))
       (times ,i (length ,o)
         (setf (aref ,o ,i)
               (logior ,@(collect j 0 (- n 1)
                                  `(u<< (aref ,p (prog1 ,c (incf ,c)))
                                        ,(if msb (* 8 (- n 1 j))  (* 8 j)))))))
       ,o)))

;; construct array of u4 from `c` interleaved channels from `s` of `w` bytes each
(defun u1-u4 (c s &key (w 1) (msb t))
  (declare (optimize (safety 0) (speed 3))
           (fixnum c w)
           (type (SIMPLE-ARRAY u1 (*)) s))
  (when (and (= w 1) (< c 5))
    (return-from u1-u4
      (if msb
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u1-u4-m ,i s t))))
          #.`(cond ,@(collect i 1 4 `((= c ,i) (u1-u4-m ,i s nil)))))))
  (let* ((h (truncate (truncate (length s) c) w))
         (d (vec (* w h) u4))
         (i 0)
         (j 0))
    (declare (fixnum h i j))
    (macrolet ((m (msb)
                 `(times y h
                    (times k c
                      (times x w
                        (let ((v (aref s i)))
                          (setf (aref d j) (uior (aref d j)
                                                 (u<< v ,(if msb
                                                             `(* 8 (- c k 1))
                                                             `(* 8 k)))))
                          (incf i)
                          (incf j)))
                      (decf j w))
                    (incf j w))))
      (if msb (m t) (m nil))
      d)))




(define-condition gfx-load-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Error loading image: ~a" (text condition)))))

(defmacro gfx-load-error (fmt &rest args)
  `(error  'gfx-load-error :text (format nil ,fmt ,@args)))


(defun rect-x (x) (the fixnum (aref x 0)))
(defun rect-y (x) (the fixnum (aref x 1)))
(defun rect-w (x) (the fixnum (aref x 2)))
(defun rect-h (x) (the fixnum (aref x 3)))
(defun in-rect? (x y r)
  (declare (optimize (safety 0) (speed 3))
           (fixnum x y))
  (and (>= x (rect-x r)) (< x (rect-w r))
       (>= y (rect-y r)) (< y (rect-h r))))
(defun clip-range (p s e)
  (cond ((< p s) s)
        ((> p e) e)
        (t p)))
(defun clip-rect (r c)
  (let ((x1 (clip-range (rect-x r) (rect-x c) (rect-w c)))
        (y1 (clip-range (rect-y r) (rect-y c) (rect-h c)))
        (x2 (clip-range (+ (rect-x r) (rect-w r)) (rect-x c) (rect-w c)))
        (y2 (clip-range (+ (rect-y r) (rect-h r)) (rect-y c) (rect-h c))))
    (vector x1 y1 (- x2 x1) (- y2 y1))))

(defmacro rgb (r g b &rest a)
  (if a `(cw ,b ,g ,r ,(car a)) `(cw ,b ,g ,r)))

(defmacro w/rgb (cs c &body body)
  (with-gensyms (x)
    `(let* ((,x ,c)
            ,@(remove-if #'null (mapcar (lambda (n i)
                                           (when (string/= "_" (symbol-name n))
                                             `(,n (fb ,i ,x))))
                                         cs '(2 1 0 3))))
       ,@body)))


(defstruct gfx
  (w 1 :type fixnum) ; width
  (h 1 :type fixnum) ; heigth
  (c 4 :type fixnum) ; number of color channels
  (d (vec 1 u4) :type (SIMPLE-ARRAY u4 (*)))  ; pixels data
  (m (vec 0 u4) :type (SIMPLE-ARRAY u4 (*)))  ; color map
  (k -1 :type fixnum)  ; color key
  (hs-x 0 :type fixnum) ;hot-spot x
  (hs-y 0 :type fixnum) ;hot-spot y
  (info nil)
  )

(defmacro gfx (w h &rest args)
  (with-gensyms (ww hh c)
    `(let ((,c ,(or (getf args :c) 4))
           (,ww ,w)
           (,hh ,h))
       (make-gfx :w ,ww :h ,hh
                 :d (vec (* ,ww ,hh) u4 (if (= ,c 4) #xFF000000 0))
                 ,@args))))

(defmacro gfx-get (g x y)
  "Get pixel at x,y"
  `(let ((x ,x)  (y ,y)  (g ,g))
     (declare (fixnum x y))
     (bind-struct gfx g (d w h)
       (if (and (<= 0 x) (< x w) (<= 0 y) (< y h))
           (aref d (f+ (f* y w) x))
           0))))
(defmacro gfx-set (g x y color)
  "Set pixel at x,y to v"
    `(let ((x ,x)  (y ,y)  (g ,g) (color ,color))
       (declare (fixnum x y) (u4 color))
       (bind-struct gfx g (d w h)
         (when (and (<= 0 x) (< x w) (<= 0 y) (< y h))
           (setf (aref d (f+ (f* y w) x)) color)))
       g))

#|
(defun gfx-clear (g color)
  (declare (optimize (safety 0) (speed 3) (debug 0))
           (u4 color))
  (let ((d (gfx-d g)))
    (times i (* (gfx-w g) (gfx-h g))
      (setf (aref d i) color)))
  g)
|#

#|
#+darwin
(defun gfx-clear (g color)
  (declare (optimize (safety 0) (speed 3) (debug 0))
           (u4 color))
  (w/rgb (cr cg cb ca) color
    (let* ((d (gfx-d g))
           (l (f* (gfx-w g) (gfx-h g)))
           (v (sc u1 (vector cb cg cr ca))))
      (libc::memset_pattern4 (sb-sys:vector-sap d) (sb-sys:vector-sap v) (f* l 4))
      g)))
#-darwin
|#


(defun gfx-clear (g color)
  ;; SBCL does not do any classic loop optimizations (e.g. unrolling),
  ;; let alone any of the higher-level transforms described in Allen and Kennedy.
  ;; SBCL does rudimentary loop analysis to aid register allocation and aligning
  ;; loop heads -- but that's it.
  (declare (optimize (safety 0) (speed 3) (debug 0))
           (u4 color))
  (let* ((d (gfx-d g))
         (i 0)
         (l (f* (gfx-w g) (gfx-h g)))
         (e (uand l #x3FFFFFF0))
         (v (sc u1 #(0 255 255 0))))
    (declare (fixnum i e))
    (while (/= i e)
      #.`(progn ,@(loop as i below 16 append '((setf (aref d i) color) (incf i)))))
    (format nil "") ;; required, else sbcl would roll back loop
    (times j (- l e) (setf (aref d (f+ i j)) color))
    )
  g)

(defun gfx-hline (g color x y length)
  (declare (optimize (safety 0) (speed 3))
           (fixnum x y)
           (u4 color))
  (bind-struct gfx g (w h d)
    (when (or (< y 0) (>= y h)) (return-from gfx-hline g))
    (when (< x 0)
      (setf length (f+ x length))
      (setf x 0))
    (when (> (f+ x length) w)
      (setf length (f- w x)))
    (let* ((i (f+ (f* y w) x))
           (e (f+ i length)))
      (declare (fixnum i e))
      (while (< i e)
        (setf (aref d i) color)
        (setf i (f+ i 1)))))
  g)

(defun gfx-vline (g color x y length)
  (declare (optimize (safety 0) (speed 3))
           (fixnum x y)
           (u4 color))
  (bind-struct gfx g (w h d)
    (when (or (< x 0) (>= x w)) (return-from gfx-vline g))
    (when (< y 0)
      (setf length (f+ y length))
      (setf y 0))
    (when (> (f+ y length) h)
      (setf length (f- h y)))
    (let* ((i (f+ (f* y w) x))
           (e (f+ i (f* length w))))
      (declare (fixnum i e))
      (while (< i e)
        (setf (aref d i) color)
        (setf i (f+ i w)))))
  g)

(defun gfx-line (g color sx sy dx dy)
  (let ((x 0)
        (y 0)
        (xlen 0)
        (ylen 0)
        (incr 0)
        (p 0))
    (declare (optimize (safety 0) (speed 3))
             (fixnum sx sy dx dy x y xlen ylen incr p))
    (when (= sx dx)
      (return-from gfx-line
        (if (< sy dy)
            (gfx-vline g color sx sy (the fixnum (+ dy (- sy) 1)))
            (gfx-vline g color dx dy (the fixnum (+ sy (- dy) 1))))))
    (when (= sy dy)
      (return-from gfx-line
        (if (< sx dx)
            (gfx-hline g color sx sy (the fixnum (+ dx (- sx) 1)))
            (gfx-hline g color dx dy (the fixnum (+ sx (- dx) 1))))))
    (when (> sy dy)
      (rotatef sx dx)
      (rotatef sy dy))
    (setf ylen (- dy sy))
    (if (> sx dx)
        (setf xlen (- sx dx)
              incr -1)
        (setf xlen (- dx sy)
              incr 1))
    (setf x sx
          y sy)
    (cond
      ((> xlen ylen)
       (when (> sx dx)
         (rotatef sx dx)
         (setf y dy))
       (setf p (f- (f<< ylen 1) xlen))
       (setf x sx)
       (while (< x dx)
         (gfx-set g x y color)
         (cond ((>= p 0)
                (incf y incr)
                (incf p (f<< (f- ylen xlen) 1)))
               (t (incf p (f<< ylen 1))))
         (incf x)))
      ((> ylen xlen)
       (setf p (- (f<< xlen 1) ylen))
       (setf y sy)
       (while (< y dy)
         (gfx-set g x y color)
         (cond ((>= p 0)
                (incf x incr)
                (incf p (f<< (- xlen ylen) 1)))
               (t (incf p (f<< xlen 1))))
         (incf y)))
      ((= ylen xlen) ; diagonal line
       (while (/= y dy)
         (gfx-set g x y color)
         (incf y)
         (incf x incr))))
    g))


(defun gfx-rect (g color x y w h &key (fill nil))
  (if fill
      (let ((e (+ y h)))
        (while (< y e)
          (gfx-hline g color x y w)
          (incf y)))
      (progn (gfx-hline g color x y w)
             (gfx-hline g color x (+ y h -1) w)
             (gfx-vline g color x (+ y 1) (- h 2))
             (gfx-vline g color (+ x w -1) (+ y 1) (- h 2))))
  g)



(defun gfx-circle-empty (g color x y r)
  (let ((p (- 1 r))
        (px 0)
        (py r))
    (declare (optimize (safety 0) (speed 3))
             (fixnum x y r p px py))
    (while (<= px (+ py 1))
      (gfx-set g (+ x px) (+ y py) color)
      (gfx-set g (+ x px) (- y py) color)
      (gfx-set g (- x px) (+ y py) color)
      (gfx-set g (- x px) (- y py) color)
      (gfx-set g (+ x py) (+ y px) color)
      (gfx-set g (+ x py) (- y px) color)
      (gfx-set g (- x py) (+ y px) color)
      (gfx-set g (- x py) (- y px) color)
      (cond ((< p 0) (incf p (f+ (f* 2 px) 3)))
            (t (incf p (+ (f* 2 (f- px py)) 5))
               (decf py)))
      (incf px))))


(defun gfx-circle-filled (g color x y r)
  (let ((p (- 1 r))
        (px 0)
        (py r))
    (declare (optimize (safety 0) (speed 3))
             (fixnum x y r p px py))
    (while (<= px py)
      (gfx-vline g color (f+ x px)     y    (f+ py 1))
      (gfx-vline g color (f+ x px) (f- y py)    py   )
      (when (/= px 0)
        (gfx-vline g color (f- x px)     y     (f+ py 1))
        (gfx-vline g color (f- x px) (f- y py)     py   ))
      (if (< p 0)
          (incf p (f+ (f* 2 px) 3))
          (progn (incf p (f+ (f* 2 (f- px py)) 5))
                 (decf py)
                 (when (>= py px)
                   (gfx-vline g color (f+ (+ x py) 1)     y     (f+ px 1))
                   (gfx-vline g color (f+ (+ x py) 1) (f- y px)     px   )
                   (gfx-vline g color (f- (- x py) 1)     y     (f+ px 1))
                   (gfx-vline g color (f- (- x py) 1) (f- y px)     px   ))))
      (incf px))))

(defun gfx-circle (g color x y r &key (fill nil))
  (if fill
      (gfx-circle-filled g color x y r)
      (gfx-circle-empty g color x y r)))

(defun gfx-avg (g)
  "average color of tile (use for game minimaps)"
  (let* ((d (gfx-d g))
         (m (gfx-m g))
         (ar 0)
         (ag 0)
         (ab 0)
         (l (length d))
         (n 20))
    (declare (optimize (safety 0) (speed 3))
             (fixnum l n ar ag ab))
    (when (= l 0) (return-from gfx-avg 0))
    (times _ n
      (let ((c (aref d (random l))))
        (when m (setf c (aref m c)))
        (w/rgb (r g b) c
          (incf ar r)
          (incf ag g)
          (incf ab b))))
    (rgb (f/ ar n) (f/ ag n) (f/ ab n))))

(defun gfx-margins (g)
  "get rect surrounding sprite"
  (let* ((w (gfx-w g))
         (h (gfx-h g))
         (d (gfx-d g))
         (m (gfx-m g))
         (x1 w) (x2 -1)
         (sx 0)
         (xb w) (xe 0)
         (yb h) (ye 0))
    (declare (optimize (safety 0) (speed 3))
             (fixnum w h x1 x2 sx xb xe))
    (times y h
      (setf sx (f* y w))
      (setf xb w)
      (setf xe -1)
      (times x w
        (let ((c (aref d (f+ x sx))))
          (when m (setf c (aref m c)))
          (w/rgb (_ _ _ a) c
            (when (/= a 255)
              (when (= xb w) (setf xb x))
              (setf xe x)))))
      (when (/= xe -1)
        (when (= yb h) (setf yb y))
        (if (< xb x1) (setf x1 xb))
        (if (> xe x2) (setf x2 xe))
        (setf ye y)))
    (if (/= x1 w)
        (vector x1 yb (f+ (f- x2 x1) 1) (f+ (f- ye yb) 1))
        (vector 0 0 w h))))

(defun gfx-resize (g nw nh)
  (bind-struct gfx g (d w h)
    (when (or (/= w nw) (/= h nh))
      (let ((nd (vec (* nw nh) u4)))
        (setf d nd
              w nw
              h nh)))
    g))

(defmacro blitm (sv dv &body body)
  `(while (< y ey)
     (setf pd (+ (f* y dw) x))
     (setf ex (+ pd w))
     (setf ps (+ (f* sy sw) sx))
     (while (< pd ex)
       (let ((,sv (aref s ps))
             ,@(when (string/= (symbol-name dv) "_")
                 `((,dv (aref d pd)))))
         (setf (aref d pd) ,(if (cdr body) `(progn ,@body) (car body))))
       (incf pd)
       (incf ps xi))
     (incf y)
     (incf sy yi)))

;; FIXME: frequently blitted images should be converted to sprites
(defun gfx-blit (dst x y src &key
                 (Rect nil)  ; source rect
                 (FlipX nil)
                 (FlipY nil)
                 (Map nil)
                 (Cut nil)
                 )
  (unless Rect (setf Rect (vector 0 0 (gfx-w src) (gfx-h src))))
  (let* ((cx 0) (cy 0) (cw (gfx-w dst)) (ch (gfx-h dst)) ; destination clipping (for now hardcodded)
         ;; source clipping
         (sx (rect-x Rect))
         (sy (rect-y Rect))
         (w  (rect-w Rect))
         (h  (rect-h Rect))
         (ow w)
         (oh h)
         (xi 1) ; x increment
         (yi 1) ; y increment
         (ex 0)
         (ey 0)
         (d  (gfx-d dst))
         (dw (gfx-w dst))
         (dc (gfx-c dst))
         (s  (gfx-d src))
         (sw (gfx-w src))
         (sc (gfx-c src))
         (m  (or Map (gfx-m src)))
         (k  (gfx-k src))
         (pd 0)   ; pointer to destination
         (ps 0))  ; pointer to source
    (declare (optimize (safety 0) (speed 3))
             (fixnum x y sx sy w h ow oh ex ey xi yi sw dw pd ps cx cy cw ch)
             ((SIMPLE-ARRAY u4 (*)) m))
    ;;FIXME: following color key calculation can lead to inconsistencies
    (when (and (gfx-m src) (= k -1))
      (let ((m (gfx-m src)))
        (times i (length m) (w/rgb (_ _ _ a) (aref m i) (when (/= a 0) (setf k i))))
        (setf (gfx-k src) (if (< k 0)
                              -2
                              k))))
    (when (or (>= x cw) (>= y ch)) (return-from gfx-blit dst))
    (when (or (<= (+ x w) cx) (<= (+ y h) cy)) (return-from gfx-blit dst))
    (when (< x cx)
      (if FlipX
          (decf ow (- cx x))
          (incf sx (- cx x)))
      (setf w (- w (- cx x)))
      (setf x cx))
    (when (< y cy)
      (if FlipY
          (decf oh (- cy y))
          (incf sy (- cy y)))
      (setf h (- h (- cy y)))
      (setf y cy))
    (setf ey (+ y h))
    (when (> (+ x w) cw) (setf w (- cw x)))
    (when (> ey ch) (setf ey ch))
    (when FlipX
      (setf sx (+ sx ow -1))
      (setf xi -1))
    (when FlipY
      (setf sy (+ sy oh -1))
      (setf yi -1))
    (cond
      (cut (blitm c _ c))
      ((and (>= dc 3) (= sc 3)) (blitm c _ c))
      ((and (>= dc 3) (= sc 4))
       (blitm sv dv
         (w/rgb (dr dg db) dv
           (w/rgb (sr sg sb sa) sv
             (let ((sm (- #xFF sa))) ; source multiplier
               (setf (aref d pd)
                     (rgb (f>> (f+ (f* sr sm) (f* dr sa)) 8)
                          (f>> (f+ (f* sg sm) (f* dg sa)) 8)
                          (f>> (f+ (f* sb sm) (f* db sa)) 8))))))))
      ((and (>= dc 3) (= sc 1)) (if (< k 0)
                                    (blitm sv _ (aref m sv))
                                    (blitm sv dv (if (= sv k) dv (aref m sv)))))
      ((and (= dc 1) (= sc 1)) (if (< k 0)
                                   (blitm sv _ sv)
                                   (blitm sv dv (if (= sv k) dv sv))))
      (t (error "gfx-blit: cant blit ~a-chn onto ~a-chn image" sc dc)))
    dst))


(defun gfx-chn (g n)
  "change number of channels"
  (declare (optimize (safety 0) (speed 3))
           (fixnum n))
  (bind-struct gfx g (d c m)
    (cond ((= n c) ) ; do nothing
          ((and (= c 1) (or (= n 3) (= n 4)))
           (loop as i below (length d) do
             (setf (aref d i) (aref m (uand #xFF (aref d i))))))
          (t (error "gfx-chn cant conver ~a-chn into ~a-chn" c n))))
  (setf (gfx-c g) n)
  g)



(defvar *format-hooks* (make-hash-table :test 'equal))

(defmacro gfx-set-handler (name load make)
  `(setf (gethash ,name gfx::*format-hooks*)
         (list (lambda (x) (,load x))
               (lambda (x) (,make x)))))


(defun path-parts (path)
  (let* ((i (position path-separator path :from-end t))
         (dir (when i (subseq path 0 i)))
         (path (or (and i (subseq path (+ i 1)))
                   path))
         (i (position #\. path :from-end t))
         (file (or (and i (subseq path 0 i))
                   path))
         (ext (when i (subseq path (+ i 1)))))
    (list dir file ext)))

(defun gfx-load (s &key format)
  (unless format
    (unless (stringp s) (gfx-load-error "you must provide FORMAT for in-memory data"))
    (setf format (third (path-parts s)))
    (unless format (gfx-load-error "file has no extension; you must provide FORMAT explicitly")))
  (setf format (string-downcase format))
  (let ((h (first (gethash format *format-hooks*))))
    (unless h (gfx-load-error "no ~a loader" format))
    (when (stringp s)
      ;;(unless (file-p s) (gfx-load-error "Not a file: ~a" s))
      (setf s (load-file s)))
    (unless (consp s) (setf s (%ptr s)))
    (funcall h s)))

(defun gfx-make (d s &key format)
  (unless format
    (unless (stringp d) (gfx-load-error "you must provide FORMAT for in-memory data"))
    (setf format (third (path-parts d)))
    (unless format (gfx-load-error "file has no extension; you must provide FORMAT explicitly")))
  (setf format (string-downcase format))
  (let ((h (second (gethash format *format-hooks*))))
    (unless h (gfx-load-error "no ~a maker" format))
    (setf s (funcall h s))
    (if d
        (save-file d s)
        s)))

(defmacro colors-to-colors (&rest as)
  (let* ((i (if (symbolp (car as)) (pop as) (gensym "I")))
         (sc (pop as))
         (dc (pop as))
         (colors (pop as))
         (s (gensym "S")))
    `(let* ((,s ,colors))
       (declare (optimize (safety 0) (speed 3))
                (type (simple-array u4 (*)) ,s))
       (times ,i (length ,s)
         (w/rgb ,sc (aref ,s ,i) (setf (aref ,s ,i) ,dc)))
       ,s)))


(defmacro colors-to-bytes (sc dc colors)
  (with-gensyms (i j s d)
    `(let* ((,s ,colors)
            (,d (vec (* ,(length dc) (length ,s)) u1))
            (,j 0))
       (declare (optimize (safety 0) (speed 3))
                (type (simple-array u4 (*)) ,s)
                (fixnum ,j))
       (times ,i (length ,s)
         (w/rgb ,sc (aref ,s ,i)
           ,@(mapcar (lambda (c) `(progn (setf (aref ,d ,j) ,c) (incf ,j)))
                     dc)))
       ,d)))

(defmacro bytes-to-colors (sc dc bytes)
  (with-gensyms (i j s d)
    `(let* ((,s ,bytes)
            (,d (vec (f/ (length ,s) ,(length sc)) u4))
            (,j 0))
       (declare (optimize (safety 0) (speed 3))
                (type (simple-array u1 (*)) ,s)
                (fixnum ,j))
       (times ,i (length ,d)
         (let ,(mapcar (lambda (c) `(,c (prog1 (aref ,s ,j) (incf ,j))))
                       sc)
           (setf (aref ,d ,i) (rgb ,@dc))))
       ,d)))


(defparameter wc2-schemes
  '((red    #xA40000 #x7C0000 #x5C0400 #x440400)
    (blue   #x0C48CC #x0428A0 #x001474 #x00044C)
    (green  #x2CB494 #x14845C #x04542C #x00280C)
    (violet #x9848B0 #x742C84 #x501858 #x2C082C)
    (orange #xF88C14 #xC86010 #x983C10 #x6C200C)
    (black  #x28283C #x1C1C2C #x141420 #x0C0C14)
    (white  #xE0E0E0 #x9898B4 #x545480 #x24284C)
    (yellow #xFCFC48 #xE4CC28 #xCCA010 #xB47400)))

(defun recolor (m c)
  (setf m (copy-seq m))
  (let ((c (cdr (assoc c wc2-schemes))))
    (times i 4 (setf (aref m (+ 208 i)) (pop c))))
  m)

(defun gfx-test ()
  (let* ((g (gfx 320 240 :c 4))
         (s (gfx-load "tmp/test8.png"))
         (m (gfx-m s))
         (blue (recolor m 'blue))
         (green (recolor m 'green))
         )
    (gfx-clear g #xFFFF00FF)
    ;(gfx-set g 5 5 #xFF0000)
    ;(gfx-line g 5 5 15 20 #x0000FF)
    ;(gfx-rect g #x0000FF 5 5 10 10)
    ;(gfx-circle g #x0000FF 15 15 10 :fill nil)
    (gfx-blit g 10 10 s)
    (gfx-blit g 80 80 s :Map blue)
    (gfx-blit g 160 10 s :Map green)
    (gfx-make "tmp/1.png" g)
    nil
    ))
