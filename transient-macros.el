;;; transient.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'transient)

(defconst fmt-as-bool-pair '("T" . "F"))

(defvar transient-quit!
  [
   ""
   [("q" "Quit Local" transient-quit-one)]
   [("Q" "Quit Global" transient-quit-all)]
   [("|" "Quit" transient-quit-all)]
   ]
  " Reusable simple quit for transients "
  )

(defclass transient-macro--group (transient-prefix)
  ((description :initarg :description :initform nil))
  "Prefix Subclassed to hold a description"
  )

(cl-defmethod transient-format-description :before ((obj transient-macro--group))
  "Format the description by calling the next method.  If the result
is nil, then use \"(BUG: no description)\" as the description.
If the OBJ's `key' is currently unreachable, then apply the face
`transient-unreachable' to the complete string."
  (or (funcall (oref obj description))
      (propertize "(JG BUG: no description)" 'face 'error))
)

;;;###autoload
(defun fmt-as-bool! (arg)
  " pass in a value, convert it to one of the values in `fmt-as-bool-pair` "
  (if arg (car fmt-as-bool-pair) (cdr fmt-as-bool-pair))
  )

;;;###autoload
(defun transient-args! ()
  " utility for easily getting all current transient args "
      (transient-args transient-current-command)
  )

;;;###autoload
(defun transient-args? (&optional key)
  "utility for easily testing the args in transient"
  (member (if (symbolp key) (symbol-name key) key) (transient-args transient-current-command))
  )

;;;###autoload
(defun transient-init! (arg)
  " utility for simply setting a transient init value "
  (-partial #'(lambda (val obj)
                (oset obj value val))
            arg)
  )

;;;###autoload
(defmacro transient-make-mode-toggle! (mode &optional desc key heading mode-var)
  " Macro to define a transient suffix for toggling a mode easier "
  (let* ((fullname (intern (format "transient-macro-toggle-%s" (symbol-name mode))))
         (name (let ((str (or desc (symbol-name mode))))
                 (when heading
                   (put-text-property 0 (length str) 'face 'transient-heading str))
                 str))
        (desc-fn `(lambda () (format "%-2s : %s" (fmt-as-bool! ,(or mode-var mode)) ,name)))
        )
    `(progn
       (defvar ,(or mode-var mode) nil)
       (transient-define-suffix ,fullname ()
               :transient t
               ,@(when key (list :key key))
               :description ,desc-fn
               (interactive)
               (,mode 'toggle)
               )
       (quote ,fullname)
       )
     )
  )

;;;###autoload
(defmacro transient-make-var-toggle! (name var &optional desc key)
  " Macro to define a transient suffix for toggling a bool variable "
  (let* ((fullname (intern (format "transient-macro-toggle-%s" (symbol-name name))))
         (desc-fn `(lambda () (format "%-2s : %s" (fmt-as-bool! ,var) ,(or desc (symbol-name name)))))
         )
    `(progn
       (defvar ,var nil)
       (transient-define-suffix ,fullname ()
         :transient t
         :description ,desc-fn
         ,@(when key (list :key key))
         (interactive)
         (setq ,var (not ,var))
         )
       (quote ,fullname)
       )
    )
  )

;;;###autoload
(cl-defmacro transient-make-call! (name key fmt &body body)
  " create a transient suffix of `name`
with a string or format call, which executes the body
 "
  (let ((fullname (intern (format "transient-macro-call-%s" (if (stringp name) name
                                                           (symbol-name name)))))
        )
    `(progn
       (transient-define-suffix ,fullname ()
         :transient t
         ,@(when key (list :key key))
         :description (lambda () ,fmt)
         (interactive)
         (with-current-buffer transient--original-buffer
           ,@body
           )
         )
       (quote ,fullname)
       )
    )
  )

;;;###autoload
(cl-defmacro transient-make-subgroup! (name bind docstring &body body &key (desc nil) &allow-other-keys)
  " Make prefix subgroup bound to const `name`, as the triple (keybind descr prefix-call),
which can then be included in other transient-prefixes as just `name`'
 "
  (let ((prefix (gensym))
        (docfn (gensym))
        (doc (let ((str (or desc (symbol-name name))))
                (put-text-property 0 (length str) 'face 'transient-heading str)
                str)))
    (when (keywordp (car body))
      (pop body) (pop body)
      )
    `(progn
       (transient-define-prefix ,prefix ()
         ,docstring
         ,@body
         transient-quit!
         )
       (defun ,docfn nil ,doc)
       (defconst ,name (list ,bind  (quote ,docfn) (quote ,prefix)))
       (quote ,name)
       )
    )
  )

(provide 'transient-macros)
