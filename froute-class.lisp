(in-package :froute)



(defclass froute-class (standard-class)
  ((route :initarg :route
          :accessor route)))

(defmethod validate-superclass ((class froute-class) (super-class standard-class))
  t)



(defvar *routes* '())
(defvar *route-classes* '())



(defmethod initialize-instance :after ((class froute-class) &key)       
  "After a froute-class class is defined, we need to push it into our routes list."
  (c2mop:ensure-finalized class)
  (setf *route-classes* (push class *route-classes*))
  (build-routes))                                       

(defmethod reinitialize-instance :after ((class froute-class) &key)     
  "Replace the old class definition with the new one, then rebuild the routes."
  (c2mop:ensure-finalized class)
  (flet ((replace-class (class*)
           (if (eq (class-name class)
                   (class-name class*))
               class
               class*)))
    (setf *route-classes* (mapcar #'replace-class *route-classes*)))
  (build-routes))                                    

(defgeneric run (route method)
  (:documentation "Generic method that gets run on a matching route."))

