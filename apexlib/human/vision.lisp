;;;-*- Mode: Lisp; Package: :cl-user -*-
;;;
;;; Vision resource
;;; apex/apexlib/human/vision.lisp
;;;
;;; Copyright:      See apex/LICENSE
;;; Version:        $Id: vision.lisp,v 1.8 2006/01/15 03:42:50 dalal Exp $

(in-package :user)

;;; --------------------------------------------------------------------------
;;; Vision
;;; --------------------------------------------------------------------------

;;; The basic function of the vision model is to determine what visual
;;; info is available to action-selection processes.  It takes as
;;; input a set of visobs structures representing all nearby visible
;;; objects, each represented as a list of attributes.  Processing is
;;; modulated by a visual memory consisting of propositions describing
;;; recent observations.  The model determines what information is
;;; currently available, compares to the contents of visual memory,
;;; and then produces two kinds of output.  (1) the visual memory is
;;; modified to replace obsolete info and to add entirely new
;;; info. (2) A set of cogevents describing recent observations and
;;; their relation to visual memory are generated.  Cogevents signal
;;; action-selection mechanisms, and may thus produce a response.

;;; PERFORMANCE PREDICTION

;;; The VISOB structures that make up a physical environment's
;;; visual-field are implicitly a set of propositions about that
;;; object's appearance.  The job of the vision model is primarily to
;;; filter the set of potentially available propositions, and pass the
;;; allowed subset on to action-selection (distortion is also
;;; possible).  The basic approach is to expect prespecified kinds of
;;; visual information --- color, orientation, shape, and so on ---
;;; and then use attribute-specific mechanisms to determine
;;; availability.  Attribute values are represented in some
;;; predetermined way.  For example, orientation values range from
;;; 0-360 degrees (or to 180 if the object is symmetric across its
;;; short axis).

;;; Mechanisms for determining availablility take several kinds of
;;; information into account, include especially the state of the GAZE
;;; resource.  The vision module requires that the model of gaze
;;; specify four values: fixation (a 2d viewer-centered value defining
;;; angle of foveation); attention (a single visual object); interest
;;; (NIL or a feature-type/value pair); and the amount of time gaze
;;; has been held in its current state.  In addition, these mechanisms
;;; may use other properties of the visual object and its
;;; surroundings.  For example, perceived color may depend on the
;;; luminosity of the object and its background.

;;; In addition to predicting availability, the vision model also
;;; maintains a visual memory containing visual-object-files. making
;;; it possible to annotate new information with its relation to
;;; previous perception.  In particular, an object can have any of the
;;; following annotations: new (no information about this attribute of
;;; this object was previously observed), refreshed (the new info
;;; confirms the old), revalued (the info makes the old obsolete),
;;; refined (the new info is more specific than the old).  Available
;;; propositions about the visual field, along with annotations, are
;;; comumunicated to action selected mechanisms as "cognitive events,"
;;; or cogevents.  These will sometimes provoke an overt response, or
;;; alternately a covert one such as inference and problem-solving.

;;; Some conceptual omissions: Observability of vis events does not
;;; depend on rate of change of visual attributes (esp. slow vs. fast
;;; luminosity changes); there is no support for modeling the physical
;;; state of the eye, e.g. reduced detection of low luminosity after
;;; exposure to bright field; there is no support for individual
;;; differences in (e.g.) acuity, color-perception; only weak support
;;; for modeling possible confusion of object identity, as might
;;; result from reexamining an area with similar objects; the model
;;; does not compute properties of object groups

;;; MODELING PRINCIPLES:

;;; (1) Model performance, not process.  In other words, the model is
;;; only concerned with what visual information is available to action
;;; selection, not on how human vision actually functions.  There is
;;; no attempt to get visual availability predictions to emerge from
;;; bottom-up processing of visual info

;;; (2) As much as possible, the appearance of visual objects should
;;; be specified in terms that correspond to controllable design
;;; features -- e.g. a display icon's color in symbolic or RGB values.

;;; (3) Overpredict performance rather than underpredict.  If the
;;; model can't see things that a human easily could, that will
;;; restrict the environs it can operate in.  Corrolary: performance
;;; limitations should only be imposed when the strategies and
;;; mechanisms used to cope with those limits have also been
;;; represented.  E.g. only model limited acuity in conj with visual
;;; scanning.

;;; This library is built on the Visob library.

(require-apex-library "visob")


;;;--------------------------------------------------------------------------
;;; The Vision Resource
;;;--------------------------------------------------------------------------

;;; This macro created because the function DELETE doesn't remove
;;; the first item of a list.

(defmacro delete2 (item var) `(setf ,var (remove ,item ,var)))

(defclass vision (resource)
  ((memory :accessor memory :initarg :memory :initform nil)))

;;; Seeing determines what visible information (from a locale's visual
;;; field) is currently available to action selection.  Availability
;;; is restricted by gaze attributes (e.g. what is currently attended)
;;; and by innate properties of the vision model (limited ability to
;;; discriminate similar colors,..).  Seeing is a cyclic process for
;;; updating a visual memory store (and generating cogevents for the
;;; ASA).  For efficiency, most updates are partial, meaning that only
;;; changes to the visual field are processed.  The param <fullcycle>
;;; determines the number of cycles between full updates.

#||  Not used

(primitive
 (index (see))
 (locals
  (update-interval (500 ms))
  (cycle-count 0)
  (fullcycle 20))
 (on-start
  (full-visual-update +self+))
 (update-interval update-interval)
 (on-update
  (let* ((last-update (- (current-time) (parse-time update-interval))))
    (incf cycle-count)
    (cond ((or (= cycle-count fullcycle)
	       (new-gaze-state? (gaze +self+) last-update)
	       (new-visual-info? (locale +self+) last-update))
	   (full-visual-update +self+)
	   (setq cycle-count 0))
	  (t (partial-visual-update +self+ last-update)))
    (if (cogevents +self+) (asamain +self+)))))
||#

(defclass seeing (resource-activity)
  ((vision-resource :accessor vision-resource :initarg :vision-resource)
   (fullcycle :accessor fullcycle :initarg :fullcycle :initform 20)
   (cycle-count :accessor cycle-count :initarg :cycle-count :initform 0)))

(defmethod initialize-activity ((action seeing) (doer human))
  (declare (ignorable action))
  (full-visual-update doer))

;;; -- Do a full or partial update  

(defmethod update-activity ((seeing-1 seeing) (human-1 human))
  (let* ((last-update (- (current-time) (update-interval seeing-1))))
    (incf (cycle-count seeing-1))
    (cond ((or (= (cycle-count seeing-1) (fullcycle seeing-1))
	       (new-gaze-state? (gaze human-1) last-update)
	       (new-visual-info? (locale human-1) last-update))
	   (full-visual-update human-1)
	   (setf (cycle-count seeing-1) 0))
	  (t (partial-visual-update human-1 last-update)))
    (if (cogevents human-1) (asamain human-1))))


;;; non-nil if the state of the gaze resource has changed since last update
(defun new-gaze-state? (gaze-resource last-update)
  (if (history gaze-resource)
      (<= last-update (timestamp (first (history gaze-resource))))))

;;; non-nil if any visual event occurred since last update
(defun new-visual-info? (locale last-update)
  (loop for event in (history locale)
      until (> last-update (timestamp event))
      if (let ((event-type (first (content event))))
	   (or (member event-type native-vis-attribute-types)
	       (member event-type special-vis-attribute-types)))
      return (content event)))

;;; returns all visual events occurring since last update
(defun all-new-visual-info (locale last-update)
  (loop for event in (history locale)
      until (> last-update (timestamp event))
      if (let ((event-type (first (content event))))
	   (or (member event-type native-vis-attribute-types)
	       (member event-type special-vis-attribute-types)))
      collect (content event)))

;;; ---------------------------------------------------------------------
;;; ---- Full and partial updates

(defun full-visual-update (human) 
  (let* ((vfield (vfield (locale human)))
	 (gaze (gaze human))
	 (fix (fixation gaze))
	 (attn (locus gaze))
	 (interest (feature gaze))
	 (intvl (t-held gaze)))
    (labels 
	((process-vattributes (visobs att-types)
	   (loop for visob in visobs append
		 (let ((obfile (get-visobfile visob human))
		       (alevel (compute-attention-level attn visob))
		       (angle (view-angle (pos visob) fix human)))
		   (loop for att in att-types
		       when (process-vis-attribute visob obfile att 
				  angle alevel interest intvl human vfield) 
		       append it)))))
      (let* ((given-atts
	      (process-vattributes vfield given-vis-attribute-types))
	     (derived-atts
	      (process-vattributes 
	       vfield
	       (append derived-vis-attribute-types 
		       special-vis-attribute-types)))
	     (all-atts (append given-atts derived-atts)))
	(generate-vision-events all-atts human)))))

;;; notes: need to run through all visobs twice because no derived attributes
;;; should be handled until all given ones are.  Processing attributes
;;; side-effects to visual memory.  

;;; !! to eliminate multiple reporting of conjunctive vis events, have
;;; process-vattributes return two lists, one for cogevent to report
;;; and one for it to suppress report.  Maybe make controllable by
;;; show-level parameter

;;; - locale history will be of form (slot obj newval), not (obj slot)


(defun partial-visual-update (human last-update)
  (let* ((locale (locale human))
	 (gaze (gaze human))
	 (fix (fixation gaze))
	 (attn (locus gaze))
	 (interest (feature gaze))
	 (intvl (t-held gaze)))
    (labels
      ((process-vattributes (changelist att-set)
	 (loop for entry in changelist append
	       (let* ((visob (second entry))
		      (att-types 
		       (if (equal (first entry) 'all) 
			   att-set  ;; ! maybe no need to support all
;;;			 (intersection (cdr entry) att-set :test #'equal)))
			 (list (first entry))))
		      (obfile (get-visobfile visob human))
		      (alevel (compute-attention-level attn visob))
		      (angle (view-angle (pos visob) fix human)))
		 (loop for att in att-types
		     when (process-vis-attribute visob obfile att 
						 angle alevel interest intvl human 
						 (vfield locale))
		     append it)))))
    (let* ((vfield-changes (all-new-visual-info locale last-update))
	   (given-atts
	    (process-vattributes vfield-changes given-vis-attribute-types))
	   (derived-atts
	    (process-vattributes vfield-changes
	     (append derived-vis-attribute-types special-vis-attribute-types)))
	   (all-atts (append given-atts derived-atts)))
      (generate-vision-events all-atts human)))))
		      

;;; ---------------------------------------------------------------------------
;;; Factors affecting availability of visual info

;;; --- View angle

;;; requires that gaze and object are in the z=0 plane, assumed to be that of the
;;; display.  All positions are in rectangular coordinates with (0 0 0)
;;; corresponding to the upper-left corner of the display.
  
(defun view-angle (pos1 pos2 agent)
  (if (and pos1 pos2)
      (ang-dist2 (location agent) (append pos1 '(0)) (append pos2 '(0)))
    0.0))  ;; if any pos unspecified, view-angle is minimum


;;; --- Compute attention level

;;; Determines degree of attention to specified visual object.  If the
;;; object is the current locus of attention, it is focally-attended.  If
;;; the object is contained by the current locus or if there is no locus 
;;; (meaning everything is being attended to weakly), it is broadly
;;; attended.  Otherwise the object is unattended.

(defun compute-attention-level (locus visob)
  (cond ((or (null locus)
	     (member visob (contains (visobfile-visob locus)) :test #'equal))
	 'broadly-attended)
	((equal (visobfile-visob locus) visob)
	 'focally-attended)
	(t 'unattended)))

;;; -----------------------------------------------------------------------------
;;; Determining whether visual info available

;;; ----- Process Vis Attribute

;;; Current visual propositions can have the following status values
;;; depending on their relationship to old propositions: new,
;;; refreshed (no change), refined (more specifically valued),
;;; revalued (old value obsolete), no-info (assume no change).  A
;;; separate process handles removal of information about non-visible
;;; objects from the visual buffer.  Visual attributes must have form:
;;; (attrib object value)

(defun process-vis-attribute (visob obfile att angle attn ftr intvl human vfield)
  (let* ((oldpstruc (findprop-in-obfile att obfile))
	 (oldvalue 
	  (and 
	   oldpstruc (third (proposition-prop oldpstruc))))
	 (newvalue 
	  (current-vprop-value visob att angle attn ftr intvl human vfield)))
    (when (not (equal 'no-info newvalue))  ;;no new information
      (append
       (and (member att conjunctive-vis-attribute-types)
	    (process-conjunctive-attribute-values att obfile oldvalue newvalue))
       (cond 
	((null oldpstruc)   ;;new.. no previous information
	 (add-prop (make-proposition :prop (list att obfile newvalue)) obfile)) 
	((equal oldvalue newvalue) (refresh-prop oldpstruc))  ;;no change 
	(t
	 (let ((value-relation ;; how does the old value relate to the new? 
		(compute-vis-specificity-relation oldvalue newvalue att)))
	   (case value-relation
	     (more-specific (refresh-prop oldpstruc)) ;; no change
	     (less-specific (refine-prop oldpstruc newvalue)) ;; better info
	     (independent (revalue-prop oldpstruc newvalue)) ;; changed
	     ))))))))

;;; ------
;;; note: only conjunctive value is stored in visual memory.  Individual
;;; conjuncts are separated out only to generate cogevents.

(defun process-conjunctive-attribute-values (att obj old new)
  (labels ((generate-event-forms (annot att obj valset)
	     (mapcar 
	      #'(lambda (val) (list annot (list att obj val))) valset)))
    (when (not (consp new))
      (format t "Warning: Value component of (~a ~a) of ~a should be a list.~%"
	      att new obj)
      (format t "I am coercing to list form.  Unexpected behavior may result.~%")
      (setf new (list new))
      (if (not (consp old)) (setf old (list old)))
      (setf (slot-value (visobfile-visob obj) att) (list new)))
    (let ((added (set-difference new old :test #'equal))
	  (deleted (set-difference old new :test #'equal))
	  (kept (intersection old new :test #'equal)))
      (append 
       (and added (generate-event-forms (if old 'revalued 'new) att obj added))
       (and deleted (generate-event-forms 'extinguished att obj deleted))
       (and kept (generate-event-forms 'refreshed att obj kept))))))

;;;-----------------------------------------------------------------------------
;;; ----- Current-vprop-value

;;; This function dispatches to an attribute-specific detrminer for
;;; whether the attribute is available to and thus observed by the
;;; agent.  Some attribute types are handled specially.  The vision
;;; model interprets an attribute value equal to no-info as meaning
;;; that the attribute is currently undefined for the visob.  In this
;;; case, no cogevents or visual memory entries are made for this
;;; value.

;;; Note: truvalue not computed for count and searchmap attributes
;;; unless visob is focally-attended, thus saving computational
;;; resources.

(defun current-vprop-value (visob att angle attn-level ftr intvl human vfield)
  (let ((trueval 
	 (case att
	   (count (if (equal attn-level 'focally-attended)
		      (compute-region-count visob)
		    'no-info))
	   (searchmap 
	    (if (and (equal attn-level 'focally-attended)  ;only search focally
		     (consp ftr)  ;only search if there's some feature of interest
		     (contains visob)) ;only search if >0 objects to look at
		(compute-searchmap ftr (get-visobfile visob human)) 
	          ;; generate a list of visobfiles to search among
	      'no-info))
	   (t (cond ((member att given-vis-attribute-types)
		     (let ((v (slot-value visob att)))
		       (if (equal v no-info) 'no-info v)))
		    ((member att special-vis-attribute-types) ;;user-defined
		     (let ((v (get-special-vis-attribute-value att visob)))
		       (if (equal v no-info) 'no-info v)))
		    (t (error "Visual attribute type ~a unknown" att)))))))
    (if (equal trueval 'no-info)
	'no-info
      (apply 
       (case att
	 (color #'determine-color-value)
	 (shape #'determine-shape-value)
	 (elements #'determine-elements-value)
	 (otherwise #'min-vis-constraint))
       (list trueval angle attn-level ftr intvl visob vfield)))))

;;; -----------------------------------------------------------------------------
;;; Determining the true value of an attribute
;;; -----------------------------------------------------------------------------

;;; For some attributes, the true value is given by the visob reprentation.
;;; These functions are needed for attributes that are derived from given
;;; attributes, and may further be meaningful only within the context of some
;;; agent state (e.g. searchmap is meaningful in the context of a specified
;;; visual search goal).  

;;; Other derived attributes that may someday be modeled include: homogeneity of
;;; an attended region with respect to some attribute-value (e.g. color=green) of
;;; interest; rate of change of some attribute-value, and magnitude of change.

;;; -----------------------------------------------------------------------------

;;; Generates a list of objects with some visual property that are contained
;;; within the region of the specified visual object.

;;; !! changed: needs to be tested

(defun compute-searchmap (attribute vof)
  (let ((att-type (first attribute))   ;e.g. color
	(att-val (second attribute)))  ;e.g. blue
    (mapcan  ;;look through all objects (vof2) contained by vof
     #'(lambda (vof2)  ;;..for any that have specified attribute
	 (let ((att-val2 (findprop-in-obfile att-type vof2)))
	   (if (or (equal att-val att-val2)
		   (and (consp att-val2) (member att-val att-val2)))
	       ;; handles case of conjunctive features
	       (list vof2))))
     (findprop-in-obfile 'contains vof))))

;;; Region count is number of visobs within other visob's boundaries.  Lack of
;;; defined boundaries means that the true count value is undefined.

(defun compute-region-count (visob) 
  (if (null (vertices visob))
      'no-info
    (length (contains visob))))

;;; -----------------------------------------------------------------------------
;;; Determining perceived attribute value

;;; The perceived attribute value can differ from the truevalue in three ways.
;;; (1) It can be absent (unavailable) due to limitations of the perceptual
;;; apparatus; (2) it can be represented in an imprecise way, also due to
;;; perceptual limits (note: expectations can be used to compensate for
;;; imprecision); (3) it can be distorted in some way (e.g. optical illusions).
;;; The current model only handles availability limits and a bit of imprecision.

;;; Perceived attribute values are computed differently for different attribute
;;; types, hence there may be one "determiner" function for each type.  The
;;; current model handles some in a specialized fashion, and others with a
;;; default determiner.  The default determiner generally returns the true value.

;;; The primary sources of constraint is the state of the gaze resource --
;;; esp. where gaze is fixed, what visual object file is currently being
;;; attended, what attribute-value if any is currently of interest, and how long
;;; gaze has been in its current state.  Other sources may include cognitive
;;; state (expectations, biases,..) and visual environment characteristics such
;;; as ambient light.

;;; Attention requirement levels: focal, broad, preattentive (can
;;; interact with degree of degradation/constraint).

(defun determine-color-value (trueval dist alevel aparam ainterval visob vfield)
  (min-vis-constraint trueval dist alevel aparam ainterval visob vfield))

;;; The shape is simply visob (i.e. an undistinguished visual object) if angle
;;; is greater than 45 degrees.  Note that the value of a shape attribute is a
;;; list of (possibly) multiple shape descriptors.

(defun determine-shape-value (trueval dist alevel aparam ainterval visob vfield)
  (min-vis-constraint trueval dist alevel aparam ainterval visob vfield))
  
;; -- alternative.. shape=visob after some angular threshold  
;;  (declare (ignore alevel aparam ainterval visob vfield))
;;  (cond ((>= angle 90) no-info)
;;	((> angle 45) '(visob))
;;	(t trueval)))

;;; Visob elements are substructures that may be individually significant.  An
;;; important example is a textblock whose elements are words.  The elements
;;; value is considered an ordered list.  An integer interest parameter causes
;;; this function to extract the item indexed by that value.

;;; Getting text info should probably be handled as follows: if a particular
;;; info item is targeted within the text, that item will become available if
;;; gaze/attn is maintained for a threshold time interval; non-targeted items
;;; are available probabilistically as a function of time.  If no target text is
;;; specified it is assumed that the whole text is the intended target --- a
;;; subset of the items are made available, as prob fn of time.

(defun determine-elements-value (trueval dist alevel aparam ainterval visob vfield) 
  (declare (ignore ainterval visob vfield))
  (let ((angular-threshold 180.0))  ;; imposes no angular constraint
    (if (< dist angular-threshold)
	(cond ((and (equal alevel 'focally-attended) (numberp aparam)
		    (consp trueval) (<= aparam (length trueval)))
	       (nth (- aparam 1) trueval))
	      ((null aparam) trueval))
      no-info)))

;;; The minimum constraint on detction of a visible attribute is that
;;; the attribute appears in the agent's frontal plane -- i.e. less
;;; than 90 degrees from fixation

(defun min-vis-constraint (trueval gaze-dist attn-level attn-param attn-interval
			   visob vfield)
  (declare (ignore attn-level attn-param attn-interval visob vfield))
  (let ((angular-threshold 180.0))  ;; imposes no angular constraint
    (if (< gaze-dist angular-threshold) trueval no-info)))

;;; -------------------------------------------------------------------------
;;; Visual object files
;;; -------------------------------------------------------------------------

;;; Visual object files [Kahneman and Triesman] in visual memory are
;;; one of two outputs of the vision component.  Each visobfile
;;; specifies a list of attributes for some specified object.  The
;;; kinds of attributes represented are the same as those that define
;;; a visob (see above); these may include some domain-specific visual
;;; attributes.

;;; Visobfiles can also include derived attributes.  This would be
;;; especially useful for relative spatial information such which
;;; objects are nearby, or immediately to the left of the object;
;;; currently only containment information is represented.  Other
;;; potentially valuable derived properties relating to containment
;;; include: homo-/heterogeneity of different features, the identity
;;; of exceptions to general homogeneity, types of contained objects,
;;; various relations between subregions (e.g. has more objects).
;;; Some of these will have to be added to make good predictions about
;;; (e.g.) popout, where heterogeneity of distractors partially
;;; determines whether popout occurs; and (e.g.) selecting visual
;;; search criteria based on distribution of feature types (find
;;; friend in crowd based on redness of jacket or blondness of hair,
;;; whichever is rarer).


(defclass visobfile (appob)
  ((visob ; link to "real" visual-object that obfile represents
    :accessor visobfile-visob
    :initarg :visob
    :initform nil)
   (sensortag ; vision component name for object
    :accessor visobfile-sensortag
    :initarg :sensortag
    :initform nil)
   (props ; contents of object file     
    :accessor visobfile-props
    :initarg :props
    :initform nil)))

(defmethod print-object ((x visobfile) stream)
  (format stream "~a" (visobfile-visob x)))

;;; GET-VISOBFILE
;;;
;;; If none exists, creates visobfile and adds to vbuffer.  

;;; ! not sure it makes sense to create a visobfile rather than return
;;; nil since agent may not have seen object yet.  Maybe make optional
;;; based on whether GET is called in context of processing an observed
;;; object.

(defun get-visobfile (visob human)
  (let* ((vmem (memory (vision human)))
	 (vof (find-if #'(lambda (vf) 
			   (equal visob (visobfile-visob vf))) vmem))) 
    (or vof
	(let ((newobfile 
	       (make-instance 'visobfile
                              :sensortag (visob-tag visob)
                              :visob visob)))
	  (setf (memory (vision human))
	    (cons newobfile (memory (vision human))))
	  newobfile))))

;;; !! change else cond to make-symbol from classname+id
(defun visob-tag (visob)
  (if (and (name visob) (not (equal (name visob) "unnamed")))
      (name visob)
    (id visob)))

(defmethod replace-local-names ((v visobfile) (a agent))
  (declare (ignore a))
  (visobfile-visob v))

(defun findprop-in-obfile (att obfile)
  (find-if #'(lambda (prop) (equal att (first (proposition-prop prop))))
	   (visobfile-props obfile)))

;;; Some visobs are associated with unique names.  E.g. {Human-1} might
;;; have the name BATMAN; {CAR-3} might have the name BATMOBILE.  PDL
;;; can refer to items by these names.  But to carry out an action on
;;; such an object, the name reference must be replaced with the data
;;; structure representing the object.

(defmethod lookup-unique-name (expr (human human))
  (let ((vof (find-if #'(lambda (v) (equal expr (name (visobfile-visob v))))
		      (memory (vision human)))))
    (if vof (visobfile-visob vof))))

;;; -----------------------------------------------------------------------------
;;; Determining relative specificity

;;; this function should determine whether two attribute values are
;;; more-specific, less-specific, or independent of each other.  How this
;;; determination should be made varies across attribute types.  For example,
;;; knowing that shape=dog is more specific than shape=animal requires
;;; interaction with domain-dependent memory.  Determining that an orientation
;;; in the range of 25-30 is more specific than one from 20-40 is a simple
;;; operation.

;;; No attribute specificity determiner is currently available

(defun compute-vis-specificity-relation (oldval newval attribute-type)
  (declare (ignore oldval newval))
  (case attribute-type (t 'independent))) ;;independent by default

;;; -----------------------------------------------------------------------------
;;; Generating annotated propositions  (output of vision model)

;;; Propositions communicated to action-selection (in the form of cogevents) are
;;; sent in both annotated and unannotated forms.  The functions produce the
;;; annotated form.

(defun refresh-prop (propstruc)
  (let ((prop (proposition-prop propstruc)))
    (setf (proposition-timestamp propstruc) (current-time))
    (make-annotated-proposition 'refreshed prop)))

(defun refine-prop (propstruc newval)
  (let ((prop (proposition-prop propstruc)))
    (setf (proposition-timestamp propstruc) (current-time))
    (setf (nth 2 prop) newval)
    (make-annotated-proposition 'refined prop)))

(defun revalue-prop (propstruc newval)
  (let ((prop (proposition-prop propstruc)))
    (setf (proposition-timestamp propstruc) (current-time))
    (setf (nth 2 prop) newval)
    (make-annotated-proposition 'revalued prop)))

;;; ! it's not clear that deletes should be signalled explicitly or
;;; individually for visob attributes.  Instead, focus on offsets.

(defun delete-prop (propstruc mem)
  (let ((prop (proposition-prop propstruc)))
    (delete2 propstruc (visobfile-props mem)) 
    (make-annotated-proposition 'extinguished prop)))

(defun add-prop (propstruc mem)
  (let ((prop (proposition-prop propstruc)))
    (push propstruc (visobfile-props mem))
    (make-annotated-proposition 'new prop)))

;;; returns a list of propositional forms
(defun make-annotated-proposition (annot prop)
  `((,annot ,prop)))


;;; Modals are symbols that represent the relationship between some proposition
;;; newly encoded in a memory and what was already stored there.  E.g. the modal
;;; NEW indicates that the proposition contains entirely new information; the
;;; modal REVISED indicates that something has changed.  In addition to those
;;; described here, cogevent handling mechanisms support the "extinguished,"
;;; modal which signifies that previous information is wholly obsolete but no
;;; current information is available.

(defconstant all-modals
    '(new         ;; proposition is new information
      refreshed   ;; prop confirms old information
      revalued    ;; prop replaces old (obsolete) information
      refined))   ;; prop is more specific but compatible with old info


;;; -----------------------------------------------------------------------------
;;; Vision model output

;;; Vision generates cogevents as output.  Mainly, this includes available
;;; propositions regarding the vfield in an unprocessed form (i.e. as they are
;;; represented in visobs).  If all propositions confirm previous observations
;;; (refresh events), then an event of the form (nothing-new vision) is also
;;; generated.  If a proposition containing a uniquely named visobfile, events
;;; are generated for both the visobfile and its naming symbol.

(defun generate-vision-events (vis-events agent)
  (labels
      (;; A specialized version of COGEVENT that mimics the behavior of an
       ;; older version of the current COGEVENT function, which handled
       ;; modals occurring as the first element of the event form.
       (cogevent-vision (eventform agent)
         (if (member (first eventform) all-modals)
             (cogevent (second eventform) agent
                       :attributes `(modal ,(first eventform)))
             (cogevent eventform agent))))
    (let ((anything-new? nil))
      (mapc 
       #'(lambda (event) 
           (cogevent-vision event agent)
           (multiple-value-bind (event2 varies?)
               (substitute-names event)
             (if varies? (cogevent-vision event2 agent)))
           (if (not (equal (first event) 'refreshed))
               (setf anything-new? t)))
       vis-events)
      (if (not anything-new?) (cogevent-vision '(nothing-new vision) agent)))))

(defmethod substitute-names ((expr visobfile))
  (if (name (visobfile-visob expr))
      (values (name (visobfile-visob expr)) t)
    (values expr nil)))

(defmethod substitute-names (expr)
  (cond 
   ((consp expr)
    (let ((changep nil))
      (values
       (mapcar 
	#'(lambda (item)
	    (multiple-value-bind (newitem differentp)
		(substitute-names item)
	      (if differentp (setf changep t))
	      newitem))
	expr)
       changep)))
   (t (values expr nil))))


;;; -----------------------------------------------------------------------------

;;; Todo:

;;; Observed disappearance of visob from vfield should register as
;;; offset event.  Something should happen to the visobfile.. maybe
;;; deleted but this may interact with handling of blinking or
;;; temporary disappearance (tunnel effect eg)

;;; Handle virtual visobs, such as learned subregions with no visible
;;; boundaries.  These should generate containments but not show up in
;;; contains lists (?), region counts or searchmaps.

;;; handle visob exchanges where one visob disappears and another
;;; appears in the same place.  E.g. new browser appears on top of
;;; old.  Support confusion over identity.

;;; handle occlusion.  Perhaps also be more sophisticated about
;;; containment by seeing whether all vertices (not just centroid)
;;; inside polygon

;;; homogenize style of comments in this file.

;;; if shape value specified as a symbol, make it a list

;;; make it easy for Asa to retrieve info from vmem

;;; provide user settable parameters to allow vision to be more or less
;;; restrictive 
