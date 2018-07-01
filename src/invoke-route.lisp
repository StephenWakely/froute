(in-package :froute)


(defun make-keyword (str)
  (intern (string-upcase (subseq str 1)) :keyword))

(defun is-variable-param (segment)
  "Is this route segment a variable (does it start with :)"
  (eq (char segment 0) #\:))

(defun is-wildcard-variable (segment)
  "Is this route segment a wildcard. ie Should it return the rest of the route?"
  (eq (char segment (1- (length segment))) #\*))

(defun remove-wildcard (segment)
  "Removes the * from the end of the var"
  (if (eq (char segment (1- (length segment))) #\*)
      (subseq segment 0 (1- (length segment)))
      segment))

(defun join-segments (segments)
  "Rejoin the segments separated by /"
  (format nil "~{~a~^/~}" segments))

(defun add-to-vars (var path vars)
  (cons 
   (if (listp path)
       `(,(make-keyword var) . ,(join-segments path))
       `(,(make-keyword var) . ,path))
   vars))

(defun match-segments (route path vars)
  (cond
    ((and route path)
     (if (is-variable-param (car route))
         ;; We have a variable
         (if (is-wildcard-variable (car route))
             (values (add-to-vars (remove-wildcard (car route))
                                  path vars) 
                     nil
                     t)
             (match-segments (cdr route) (cdr path) 
                             (add-to-vars (car route) (car path) vars)))
         ;; We just have a match
         (if (string-equal (car route) (car path))
             (match-segments (cdr route) (cdr path) vars)
             (values nil nil nil))))
    ((and (not route) (not path))
     (values vars nil t))
    (path
     (values vars path t))
    (t ;; We cant match all the request
     (values nil nil))))

(defun trim-slashes (route)
  "Removes leading and trailing /"
  (string-trim '(#\/) route))

(defun split-route (route)
  "Splits the route up into its constituent parts."
  (cl-ppcre:split "/" 
                  (trim-slashes route)))

(defun route-matches (route method request-route request-method)
  (when (member method (list :any request-method))
    (let ((split-route (split-route route))
          (split-request-route (split-route request-route)))
      (match-segments split-route split-request-route '()))))


(defun create-route-instance (class params)
  "Creates the route class and populates its slots with the variables."
  (let ((slots (class-slots class))
        (instance (make-instance class)))
    (loop for slot in slots
         do
         (let ((name (slot-definition-name slot)))
           (setf (slot-value instance name)
                 (cdr (assoc name params :test #'string-equal)))))
    instance))

(defun walk-routes (path method routes vars)
  "Walk down the routes tree matching the paths and collecting
any variables along the way. "
  (if routes
    (let ((head (car routes)))
      (let ((split-route (split-route (car (route (route-class head))))))
        (multiple-value-bind
              (vars* remaining matched)
            (match-segments split-route path '())
          (if matched
              (if remaining
                  (walk-routes remaining method (route-children head) (append vars vars*))
                  (values (append vars vars*) head))

              (walk-routes path method (cdr routes) vars)))))
    (values nil nil)))

(defun invoke-route (path &key (method :any))
  (let ((split (split-route path)))
    (multiple-value-bind
          (vars matched)
        (walk-routes split method *routes* '())
      (when matched
        (run (create-route-instance (route-class matched) vars) method)))))

