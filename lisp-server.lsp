(ql:quickload :usocket)
(ql:quickload :trivial-timers)

(defstruct node 
  (state-name 0 :type integer)
  (state '() :type list)
  (edges '() :type list))

(defstruct mdpr 
  (states '() :type list) 
  (actions '() :type list)
  (cur-state 0)
  (start-state 0)
  (graph '() :type list)
  (gamma 0.0 :type short-float))

(defstruct option 
  (name 'nil :type symbol)
  (policy '() :type list)
  (termination-conditions '() :type list))

(defstruct term-cond
  (state-name 'nil :type symbol)
  (termination-prob 0.00 :type short-float))

(defstruct state
  (name '() :type integer)
  (data '() :type list))

(defstruct action
  (sub-actions '() :type list))

(defvar *thread-variables* '())

(defvar *atomic-actions* '())
(defvar *key* (make-hash-table))
;(setf (gethash 1 *key*)'attack)
;(setf (gethash 2 *key*) 'barter)
(setf (gethash 1 *key*)'create)
;(setf (gethash 4 *key*)'escort)
;(setf (gethash 5 *key*)'gather)
;(setf (gethash 6 *key*)'graze)
(setf (gethash 2 *key*)'heal)
(setf (gethash 3 *key*)'lock)
(setf (gethash 4 *key*)'loot)
(setf (gethash 5 *key*)'move)
;(setf (gethash 11 *key*)'patrol)
(setf (gethash 6 *key*)'repair)
;(setf (gethash 13 *key*)'scout)
(setf (gethash 7 *key*)'trade)
(setf (gethash 8 *key*)'military-units) 
;(setf (gethash 16 *key*)'cavalry-units) 
(setf (gethash 9 *key*)'support-units) 
;(setf (gethash 18 *key*)'siege-units) 
;(setf (gethash 19 *key*)'naval-units) 
(setf (gethash 10 *key*) 'structures)
(setf (gethash 11 *key*) 'resources)

(defvar *entities* '((make-entity :name 'infantry-units :type 'infantry :owner '())
		    (make-entity :name 'cavalry-units :type 'cavalry :owner '())
		    (make-entity :name 'support-units :type 'support :owner '())
		    (make-entity :name 'siege-units :type 'siege :owner '())
		    (make-entity :name 'naval-units :type 'ship :owner '())
		    (make-entity :name 'structures :type 'structure :owner '())
		    (make-entity :name 'resources :type 'resourc :owner '())))

(defun tcp-test-client (port)
  (setq conn (usocket:socket-connect usocket:*wildcard-host* port)))

(defun tcp-test-send ()
  (let ((stream (usocket:socket-stream conn)))
  (format stream "ABCD Hello World!~%")
  (force-output stream)
  (read-line stream nil)))

(defun tcp-server (port) 
  ; clear residual thread variables
  (setq *thread-variables* '())
 
  (let ((socket (usocket:socket-listen usocket:*wildcard-host*
				       port
				       :reuse-address t))
	(count 0))
    (unwind-protect 
	 (progn
	   (loop 
	      (format *standard-output* "Waiting for input on ~A~%" socket)
	      (usocket:wait-for-input socket)
	      (let* ((connection (usocket:socket-accept socket))
		     (stream (usocket:socket-stream connection))
		     (thread-index count))
		
		(format *standard-output* "Connection made to client ~%")
		(setq *thread-variables* 
		      (append *thread-variables* (list (list '() t))))
		  
                  ; handle the request on thread and let main accept new clients
		  (sb-thread:make-thread 
		   (lambda(std-out cnt)
		     (let* ((*standard-output* std-out)
			    (timer (trivial-timers:make-timer 
				    #'(lambda ()
					(format *standard-output* 
						"Connection timeout. Closing ~A.~%" 
						connection)
					(usocket:socket-close connection)	 
					(setf (second (nth cnt *thread-variables*)) '())
					(setf (first (nth cnt *thread-variables*)) '())
					(sb-thread:terminate-thread sb-thread:*current-thread*)))))
		       
		       ; 5 minute timeout
		       (trivial-timers:schedule-timer timer (* 5 60))
		     
		       (loop while (not (null (second (nth cnt *thread-variables*)))) do
			    (handle-request stream cnt std-out timer)
			    (clear-input stream)))) 
		   :arguments (list *standard-output* thread-index))
		  (incf count))))
      (progn
	(usocket:socket-close socket)))))

#| Service a request from a client |#

; stream = socket stream to client
; t-idx = index of thread variables for thread
; ostream = reference to standard out
; timer = timeout timer 
(defun handle-request (stream t-idx ostream timer)
  (trivial-timers:schedule-timer timer (* 5 60))
  (let ((client-id (write-to-string (read stream))))
    (with-open-file (clientData (concatenate 'string 
					     "clientData/"
					     client-id ".txt")
				:direction :output
				:if-exists :append
				:if-does-not-exist :create)
      (let ((line (read-line stream nil 'the-end))
	    (*standard-output* ostream))
	(setf (first (nth t-idx *thread-variables*)) t)
	(map '() 
	     #'(lambda (feature)
		 (format clientData "~S~T" feature))
	     (with-input-from-string (in line)
	       (loop for x = (read in nil nil)
		  while x collect x)))
	(format clientData "~%")
	(force-output clientData)
	(format *standard-output* "Received: ~S~%" line)))
    (let ((result (read-line (sb-ext:process-output 
			      (sb-ext:run-program 
			       "python" 
			       (list "/home/ec2-user/INTENT-R/MachineLearning/predict.py" client-id) 
			       :search t 
			       :wait '() 
			       :output :stream 
			       :error :stream)) 
			     '())))
      (format stream "~S~%" result)
      (force-output stream))))
    

(defun to-syms (inp)
  (let ((syms '()))
    (with-input-from-string (x inp)
      (do ((word (read x nil '()) (read x nil '())))
	  ((null word))
	(push word syms)))
    (reverse syms)))

#| Initialize MDP and expert's feature expectations |#

; init-file = states, transition probabilities, m trajectories
; return reward function   
(defun init-apprentice (init-file)
  ; create MDP/R
  (let ((mdp-r (make-mdpr :states '()
			  :actions '()
			  :graph '()
			  :cur-state 0
			  :start-state 0)))
    
    ; update :owner for each player in game
    ; fill state/action space
    (setf (mdpr-states mdp-r) (make-state-space '()))
    ;(setf (mdpr-actions mdp-r) (enumerate-action-space))
    (setf (mdpr-actions mdp-r) (test-enum-actions))

    ; create transition probabilites
    (setf (mdpr-graph mdp-r) (make-graph mdp-r))
    ; create expert's feature expectations

    ;return reward function
    ;(act mdp-r)
    
    ;(discover-reward mdp-r '() '())
  ))

#| Define the state space for the MDP |#

; states = state list to fill
(defun make-state-space (states)
  (with-open-file (client-data "testStates.txt"
			       :direction :input
			       :if-does-not-exist :error)
    (let ((count 0))
      
      (do ((line (read-line client-data nil) 
		 (read-line client-data nil)))
	  ((null line))
	(let ((list (to-syms line)))
	  ;(setq states (reverse (cons (make-state :name count :data list)
	  (setq states (reverse (cons list
				      (reverse states)))))
	(incf count)))
    states))

#|Determine action space in terms of codes|#

; (action with what who)
(defun enumerate-action-space ()
  (let ((acc '()))
    (do ((i 1 (incf i)))
	((= i 11))
      (do ((j 0 (incf j)))
	  ((= j 11))
	(do ((k 0 (incf k)))
	  ((= k 11))
	  (do ((l 0 (incf l)))
	      ((= l 11))
	    (let ((unfiltered (copy-list '(0 0 0 0))))
	      (setf (nth 0 unfiltered) i)
	      (setf (nth 1 unfiltered) j)
	      (setf (nth 2 unfiltered) k)
	      (setf (nth 3 unfiltered) l)
	      (push unfiltered acc))))))
    (let ((actions (remove-duplicates (remove-if #'(lambda (list)
						     (if (or (and (> (nth 1 list) 0)
								  (<= (nth 1 list) 7))
							     (and (> (nth 2 list) 0)
								  (<= (nth 2 list) 7))
							     (and (> (nth 3 list) 0)
								  (<= (nth 3 list) 7))
							     (eq 10 (nth 3 list))
							     (eq 11 (nth 3 list))
							     (>  (nth 0 list) 7)
							     (and (<= (nth 0 list) 7)
								 (> (nth 1 list) 9))
							     (and (= 1 (nth 0 list))
								  (or (> (nth 1 list) 0)
								      (> (nth 3 list) 0)
								      (< (nth 2 list) 8)
								      (= 11 (nth 2 list))
								      (= 0 (nth 2 list))))  
							     (and (eq 2 (nth 0 list))
								  (or (not (= 9 (nth 1 list)))
								      (> (nth 2 list) 0)
								      (> (nth 3 list) 9)
								      (= 0 (nth 3 list))))
							    (and (eq 3 (nth 0 list))
								 (or (> (nth 1 list) 0)
								     (> (nth 3 list) 0)
								     (not (= 10 (nth 2 list)))
								     (= 0 (nth 2 list))))
							    (and (eq 4 (nth 0 list))
								 (or (> (nth 1 list) 0)
								     (> (nth 3 list) 0)
								     (not (= 11 (nth 2 list)))
								     (= 0 (nth 2 list))))
							    (and (eq 5 (nth 0 list))
								 (or (> (nth 1 list) 0)
								     (> (nth 2 list) 0)
								     (not (or (= 8 (nth 3 list))
									      (= 9 (nth 3 list))))
								     (= 0 (nth 3 list))))
							    (and (eq 6 (nth 0 list))
								 (or (> (nth 1 list) 0)
								     (> (nth 3 list) 0)
								     (not (= 10 (nth 2 list)))
								     (= 0 (nth 2 list))))
							    (and (eq 7 (nth 0 list))
								 (or (> (nth 1 list) 0)
								     (> (nth 3 list) 0)
								     (= (nth 2 list) 0)
								     (= (nth 2 list) 10)))) 
							t
							'()))
						acc) :test 'equal)))
      (nconc (combinations 2 actions)
	     (combinations 1 actions)
	     (combinations 0 actions)))))
  

(defun test-enum-actions ()
  (let ((actions '((1 2 3 4) (5 6 7 8))))
    (nconc (combinations 2 actions)
	   (combinations 1 actions)
	   (combinations 0 actions))))
#|Perform n choose count combination |#

; list = list to perform combinations on
; count = choose
(defun combinations (count list)
  (cond
    ((zerop count) '(())) ; one combination of zero element.
    ((endp list)   '())   ; no combination from no element.
    (t (nconc (mapcar (let ((item (first list))) 
			(lambda (combi) 
			  (cons item combi)))
                      (combinations (1- count) (rest list)))
              (combinations count (rest list))))))


#| build transition graph: 
   (([A -> A] . % [A -> B] . % ... [A -> N] . %)
    ([B -> A] . % [B -> B] . % ... [B -> N] . %)
    .
    .
    .
    ([N -> A] . % [N -> B] . % ... [N -> N] . %))
   
   [A -> B] . % = list of all possible actions from A to B. Each action has a probability.
   . % is the probability of going from A -> B.
   
   Figuring out a move is esentially P(ai | A -> B)|#

; mdpr = mdpr simulation
(defun make-graph (mdpr)  
  (mapcar #'(lambda (state)
	      (make-transition mdpr))
	  (mdpr-states mdpr)))

(defun make-transition (mdpr)
  (mapcar
   #'(lambda (state) ; Make n x n matrix with each entry a list of possible actions
       (let ((state-row '()))
	 (setq state-row (make-action-list (mdpr-actions mdpr)))))
   (mdpr-states mdpr)))

(defun make-action-list (mdpr-actions)
  (cons (map 'list
	     #'(lambda (action)
		 (list action 0))
	     mdpr-actions) 0))

#| Assign the transition probabilities for each node 

   (([A -> A] . % [A -> B] . % ... [A -> N] . %)
    ([B -> A] . % [B -> B] . % ... [B -> N] . %)
    .
    .
    .
    ([N -> A] . % [N -> B] . % ... [N -> N] . %))
   
   [A -> B] . % = list of all possible actions from A to B. Each action has a probability.
   . % is the probability of going from A -> B.
   
   Figuring out a move is esentially P(ai | A -> B)|#

(defun make-transition-probs (mdpr))

#| Discovers reward function |#

; mdpr = mdp w/o reward funtion -- simulator
; phi = feature mapping -- (state -> state features)
; mue = expert's feature expectations
; returns reward
(defun discover-reward (mdpr phi mue)
  
  (let ((p (make-hash-table)))
    ;add elements to p
    (map '() 
	 #'(lambda (state)	    
	    (setf (gethash (state-name state) p) 
		  (nth (+ 1 (random (length (mdpr-actions mdpr)))) 
		       (mdpr-actions mdpr))))
	 (mdpr-states mdpr))
    (format t "Pi: ~A~%" p)))
    ;(mu p)))

#| Compute feature expectation of policy, pi |#

; mdpr = MDP simulator
; p = policy
; returns feature expectations of p
(defun mu (mdpr p)
  ;expected value of the sum of the discount * phi
  (let* ((err .01)
	 (gamma .5)
	 ;epsilon-horizon time
	 (He (log (* err (- 1 gamma)) gamma)))
    
    (mapcar #'(lambda (&rest x) (reduce #'+ x))  
	    (map 'list 
		 #'(lambda (st)
		     (do ((i 0 (1+ i))
			  (>= i He))
			 (map 'list 
			      #'(lambda (comp)
				  (* (expt gamma i) comp)) 
			      st)))
		 (act p)))))
  
#| Get a random percent |#

(defun random-percent ()
  (/ (random 101) 100))

#| Simulate an action in the MDP-R |#

; mdpr = mdpr
; returns state
(defun act (mdpr) 
  ; find all the probabilities of moving out of this state.
  (let ((possible-actions '()))
    (map 'list
	 #'(lambda (state)
	     (format t "State: ~S~%" state)
	     (loop for transition in state do
		  (loop for a in transition do
		       (format t "~S~%" (second a))
		       (if (not (= 0 (second a)))
			   collect a))))
	 (nth (mdpr-cur-state mdpr) 
	      (mdpr-graph mdpr)))))
    
    #|(let* ((normalizer (reduce #'* (map 'list #'denominator probs)))
	   (current 0)
	   (allotments (loop for upper-bound in (mapcar
						 #'(lambda (frac)
						     (* normalizer (numerator frac)))
						 probs)
			  collect (list current upper-bound))))
      allotments)))
    
  ; distribute probabilities accross dice
    (map 'list 
	 #'(lambda (frac)
	     (/ 100 (denominator frac))))
  ; roll dice
    (random 101)

  ; Follow transition
  )
|#

(defun test-act () 
  ; find all the probabilities of moving out of this state.
  (let* ((graph '(((((((1 2 3 4) (5 6 7 8)) 1/9) (((1 2 3 4)) 0) (((5 6 7 8)) 1/9) (NIL 0)) . 1/3)
		   (((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 1/9) (((5 6 7 8)) 3/9) (NIL 0)) . 0)
		   (((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 2/9) (((5 6 7 8)) 3/9) (NIL 0)) . 2/3))
		  ((((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 0) (((5 6 7 8)) 0) (NIL 0)) . 0)
		   (((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 0) (((5 6 7 8)) 0) (NIL 0)) . 0)
		   (((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 0) (((5 6 7 8)) 0) (NIL 0)) . 0))
		  ((((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 0) (((5 6 7 8)) 0) (NIL 0)) . 0)
		   (((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 0) (((5 6 7 8)) 0) (NIL 0)) . 0)
		   (((((1 2 3 4) (5 6 7 8)) 0) (((1 2 3 4)) 0) (((5 6 7 8)) 0) (NIL 0)) . 0))))
	 (probs '())
	 (possible-moves 
	  (mapcar #'(lambda (transition)
		      (format t "Transition: ~S~%" (car transition))
		      (remove-if #'(lambda (x)
				     (zerop (second x)))
				 (loop for a in (car transition) do
				      (push (second a) probs)
				    collect a)))
		  (nth 0 graph))) ; 0 is going to be cur state of mdpr
	 (normalizer (reduce #'lcm (map 'list 
				      #'(lambda (rational)
					  (denominator rational))
				      probs)))
	 (move-list (mapcar #'(lambda (move) 
				(mnconc (mapcar #'(lambda (action)
					    (format t "~S~%" action)
					    (let ((copy-times (* (numerator (second action))
								 (/ normalizer 
								    (denominator (second action))))))
					      (format t "Copy times: ~d~%~%" copy-times)
					      (loop for i from 1 to copy-times 
						 collect (first action))))
					move)))
			    possible-moves))
	 (gen (biased-generator '(0 1 2) 
				(list (floor (/ normalizer (denominator (cdr (first (nth 0 graph)))))) 
				  (floor (/ normalizer (denominator (cdr (second (nth 0 graph))))))
				  (floor (/ normalizer (denominator (cdr (third (nth 0 graph)))))))))
	 (state-list (loop for i from  1 to 100 collect (funcall gen)))
	 (next-state (nth (random (length state-list)) state-list))
	 (choices (nth next-state move-list))) ; NEXT STATE is NULL SOMETIMES!!!!!
    (format t "State list: ~S~%" state-list)
    (format t "Next State: ~d~%" next-state)
    (format t "Moves list: ~S~%" move-list)
    (format t "Possible moves: ~S~%" possible-moves)
    (cons (if (not (null choices))
	      (nth (random (length choices)) choices)
	      choices) next-state)))

#|nconc for list of lists|#

; llist = list of list
(defun mnconc (llist)
  (if (>= 2 (length llist))
      (nconc (first llist) (second llist))
      (nconc (first llist) (mnconc (rest llist)))))

(defun biased-generator (values weights)
  (multiple-value-bind (total values)
      (loop for v in values
         for w in weights
         nconc (make-list w :initial-element v) into vs
         sum w into total
         finally (return (values total (coerce vs 'vector))))
    (lambda ()
      (aref values (random total)))))

#| SCRATCH 

; test I did for mu functionality
(mapcar #'(lambda (&rest x)
		     (reduce #'+ x)) '(1 2 3 4) '(1 2 3 4))

; server printfs
(format *standard-output* 
		 "THREAD VARIABLES ~A~%COUNT ~d~%" 
		 *thread-variables* 
		 thread-index)

(format ostream "conn-timeout: Thread Variables ~A~%conn-timeout: t-idx ~d~%" 
	    *thread-variables*
	    t-idx)

(format ostream "Using count: ~d~%" t-idx) 

; modifying plist
(let ((x '(:a 1 :b 2)))
	   (setf (getf x :a) 'artist)
	   x)|#
