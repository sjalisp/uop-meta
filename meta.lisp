;;;; uop-meta.lisp

(in-package #:uop-meta)
(in-package :cw-uop)

(defclass meta-permissions ()
  ((sys-defined :accessor sys-defined :initform nil :initarg :sysdef)
   (app-defined :accessor app-defined :initform nil :initarg :appdef)
   (modifiable :accessor modifiable :initform t :initarg :mutable)
   (deletable :accessor deletable :initform t :initarg :deletable)))


(setq system-permission (make-instance 'meta-permissions :sysdef t :mutable nil
                                                         :deletable nil))

(setq app-permission (make-instance 'meta-permissions :appdef t :mutable nil
                                                         :deletable nil))

(setq user-permission (make-instance 'meta-permissions))


(defclass by-name-id ()
  ((by-name :reader by-name :initform (make-hash-table :test string=))
   (by-id :reader by-id :initform (make-hash-table :test string=))))


(defclass with-name-id ()
  ((id :accessor id :initarg :id :type 'string)
   (name :accessor name :initarg :name :type 'string)
   (description :accessor description :initarg :description :initform "" :type 'string)
   (permissions :accessor permissions :initarg :permissions :initform user-permission :type 'meta-permissions)))

(defclass meta-class (with-name-id)
  (
   (supername :reader supername :initarg :super :initform nil :type 'string)
   (attr-ids :accessor attr-ids :initarg :attr-ids :initform () :type (array string))
   (attributes :accessor attributes :initargs :attributes :initform ())
   (short-form :accessor short-form :initargs :short-form :type (array string))
   (instance-collection :accessor instance-collection :initarg :instance-collection :type 'string)))


(defclass meta-attribute (with-name-id)
  ((class_name :accessor class-name :initarg :class-name :initform "" :type 'string)
   (a_type :accessor a_type :initarg :type :type 'string)
   (required :accessor required :initarg :required :type 'boolean)))

(defun make-permissioned-attribute (permission)
  (lambda (name &optional description)
    (make-instance 'meta-attribute :id (make-meta-oid) :name name :description description
                                   :permissions permission)))


(defun sys-attribute (name &optional description)
  ((make-permissioned-attribute sys-permission) name description))

(defun app-attribute (name &optional description)
  ((make-permissioned-attribute app-permission) name description))


(defclass meta-group (with-name-id) ())

(defclass meta-tag (with-name-id)())

(defclass meta-role (with-name-id)
  ((reverse_name :accessor reverse-name :initarg :reverse-name :type 'string)))

(defclass associated ()
  ((assoc-id :reader assoc-id :initarg :assoc-id :type 'string)
   (object-id :reader object-id :initarg :object-id :type 'string)))

(defclass tagged (associated)())
(defclass grouped (associated)())
(defclass related (associated)
  ((subject-id :reader subject-id :initarg :subject-id :type 'string)))


(defclass meta-context ()
  ((classes :reader classes :initform (make-instance 'by-name-id))
   (attributes :reader attributes :initform (make-instance 'by-name-id))
   (tags :reader tags :initform (make-instance 'by-name-id))
   (groups :reader groups :initform (make-instance 'by-name-id))
   (roles :reader roles :initform (make-instance 'by-name-id))
   (queries :reader queries :initform (make-instance 'by-name-id))))

(defclass query-component ()())

(defclass class-component (query-component)
  ((class-name :reader class-name :initarg :class)
   (include :reader include :initarg :include :initform t)
   (include-subclasses :reader include-subclasses :initarg :subclasses :initform t)))

(defclass tags-component (query-component)
  ((tags :reader tags :initarg :tags)
   (filter-as :reader filter-as :initarg :filter-as :initform :all)))

(defclass groups-component (query-component)
  ((groups :accessor groups :initarg :groups)
   (filter-as :reader filter-as :initarg :filter-as :initform :all)
   (include-contained :reader include-contained :initarg :include-contained :initform t)))

(defclass attribute-component (query-component)
  ((attr-name :reader attr-name :initarg :attr)
   (operator :reader operator :initarg :operator)
   (value :reader value :initarg :value)))

(defclass related-component (query-component)
  ((role-name :reader role-name :initarg :role)
   (subject :reader subject :initarg :subject :initform nil)
   (object :reader object :initarg :object :initform nil)))
  

(defclass composite-component (query-component)
  ((components :accessor components :initarg :components :initform nil)))

(defclass and-component (composite-component) ())

(defclass or-component (composite-component) ())

(defclass meta-query (with-name-id)
  ((component :reader component :initarg :component)))

(defclass query-eval-context ()
  ((db :reader db :initarg :db)
   (metadata :accessor metadata :initarg :metadata :initform nil)
   (object-ids :accessor object-ids :initform '())))

(defgeneric evaluate-component (eval-context component))


(defmethod evaluate-component (eval-context (component class-component) ))
(defmethod evaluate-component (eval-context (component groups-component)))
(defmethod evaluate-component (eval-context (component tags-component))
  (labels ((any-tags (oids tags))
	   (all-tags (oids tags))
	   (no-tags (oids tags)))
    (let ((oids (oids tags-component))
	  (db (db tags-component))
	  (kind (filter-as tags-component))
	  (tags (tags tags-component)))
      
      (if (not (oids component))
	  (progn
	    (setf oids (get-tag-objects db (car tags)))
	    (setf tags (cdr tags))))
      (case kind
	(:any (any-tags oids tags))
	(:all (all-tags oids tags))
	(:none (no-tags oids tags))))))
	   
	
      
(defmethod evaluate-component (eval-context (component related-component)))
(defmethod evaluate-component (eval-context (component attribute-component)))
(defmethod evaluate-component (eval-context (component and-component)))

(defmethod evaluate-component (eval-context (component or-component)))
  (reduce (lambda (a b) (union a (evaluate-component eval-context b)))
          (components component)
          oids))


(defun evaluate-query (db query &key (metadata nil))
  (let* ((meta-context (or metadata (metadata db)))
         (context (make-instance 'query-eval-context :db db
                                                     :metadata meta-context)))
    (evaluate-component context (component query))))

        
                                                      
