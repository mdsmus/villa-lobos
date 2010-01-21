(in-package :om)
(defvar *pos-last-mesure* nil)

(defun in-last-mesure (n)
 (>= n *pos-last-mesure*))

(defun corrige-first-chord (chord numvoices)
 (if (= numvoices (length (inside chord)))
    chord
  (let ((notes (sort (inside chord) '< :key 'chan))
        pitches)
    (loop for i from 1 to numvoices do
          (let ((note (member i notes :key 'chan)))
          (if note (push (midic (car note)) pitches)
            (push (midic (car notes)) pitches))))
    (make-instance 'chord
                   :lmidic (reverse pitches)
                   :lchan (arithm-ser 1  numvoices 1)))))


(defun complete-voice-chord (chord chord1 )
 (if (= (length (inside chord)) (length (inside chord1)))
    chord1
  (let ((notes (sort (inside chord) '< :key 'chan))
        (notes1 (sort (inside chord1) '< :key 'chan))
        pitches)
    (loop for i from 1 to (length (inside chord)) do
          (let ((note (member i notes1 :key 'chan)))
            (if note (push (midic (car note)) pitches)
              (push (midic (nth (- i 1) notes)) pitches))))
    (make-instance 'chord
                   :lmidic (reverse pitches)
                   :lchan (arithm-ser 1  (length (inside chord)) 1)))))


(defun get-initial-chords (poly)
 (let ((i 0)
      chord-seq chords rep
      (last-mesure (offset->ms (car (last (inside (car (voices poly)))))))
      poslast onsets)
  (loop for item in (voices poly) do
        (set-channel item (incf i)))
  (setf chord-seq (objfromobjs poly (make-instance 'chord-seq)))
  (setf onsets (print (lonset chord-seq)))
  (setf chords (inside chord-seq))
  (loop for item in chords
        for i = 0 then (+ i 1) do
        (unless poslast
          (when (>= (offset->ms item) last-mesure)
            (setf *pos-last-mesure* i))))
  (setf (nth 0 chords)  (corrige-first-chord (car chords) (length
(voices poly))))
  (loop for i from 1 to (- (length chords) 1) do
        (setf (nth i chords) (complete-voice-chord (nth (- i 1)
chords) (nth i chords) )))
  (make-instance 'chord-seq
                 :lmidic (loop for item in chords collect
                               (lmidic item))
                 :lonset onsets)))

; TO-DO Falta a@adir los criterios r@tmicos
(defmethod! analiseVoice ((self voice))
          (loop for bar in (inside self) do
                 (loop for chord in (inside bar) do
                       (print chord)
                       )
                 )
)

(defmethod analise ((self poly))
 (let* ((theVoices (voices self)))
  (loop for aVoice in (voices self) do
        (analiseVoice aVoice)
  )

))


(defmethod! analise1 ((self poly))
 (let* ((chord-seq (objfromobjs self (make-instance 'chord-seq)))
       (lista (lmidic chord-seq))
       (voces (voices self))
       (voz1 (nth 0 voces))
       (notaPrueba (add-extra self
                              (make-instance 'text-extra :thetext
"hola") '(3 0 0 0))))
 (print (mc->n (flat (loop for chord in (chords voz1) collect
          (Lmidic chord) ))))
  self
       ))



; tag each voice note as harmonic or non-harmonic tone
(defmethod! melodicAnalysis ((self poly))
)

(defmethod! debug ((self string))
          (print self)
)

; if outputs a list with the valid keys for the input chord.
; The minor keys range from 0 to 11, the major from 12 to 23
(defmethod! getValidKeys ((self list))
          (let* ((MAJOR_SCALE_VALID_SEMITONES_FROM_ROOT '(0 1 2 4 5 7
9 11)) ; 1 = napolitana
                 (MINOR_SCALE_VALID_SEMITORES_FROM_ROOT '(0 1 2 3 4 5
7 8 9 10 11))  ; 1 = napolitana, 4 picarda
                ; (MINOR_SCALE_VALID_SEMITORES_FROM_ROOT '(0 2 3 5 7
8 9 10 11)) ; URGENT, quito la tercera picarda (4) y la napolitana (1)
                 validKeys)
          (loop for key from 0 to 11 do
                ;(debug (format nil "Key=~D~%" key))
                (let (notes)
                (loop for note in self do  ; URGENT Este bucle se
podr@a poner arriba del loop de key from 0 to 11
                      (pushnew (mod (- (+ 12 note) key) 12) notes
:test #'equal) ; add only if not exists
                ) ; end loop notes
                (if (included? notes
MAJOR_SCALE_VALID_SEMITONES_FROM_ROOT) (push (+ 12 key) validkeys)) ;
major scales range from 12 to 23
                (if (included? notes
MINOR_SCALE_VALID_SEMITORES_FROM_ROOT) (push key validkeys)) ; minor
scales from 0 to 11
          )) ; end loop keys
          (sort validKeys '<)
          ;(print validKeys)
          validKeys))

;(getValidKeys '(0 4 7))
;(getValidKeys '(5 9 0))

; it returns a sequence to pitch classes into a sequence of positive
intervals (from pitch classes 7 10 2) to 3 4
(defmethod! x->positivedx ((self list))
          (mapcar #'(lambda (x) (if (< x 0) (+ 12 x) x)) (x->dx self))
)

;(x->positivedx '(7 10 2))

; it returns the type of the given chord
(defmethod! getChordType ((self list))
; each list contains the name of the chord and a list of intervals
root -> third -> fifth
 (let* ((CHORD_TYPE_INTERVALS '((maj (4 3)) (min (3 4)) (aug (4 4))
(dim (3 3)) (maj7min (4 3 3)) (aug7maj (4 4 3)) (dim7min (3 3 4))
(dim7dim 3 3 3) (maj7maj 4 3 4) (min7min 3 4 3)))
       (chordIntervals (x->positivedx self))
       chordType)
  (loop for i in CHORD_TYPE_INTERVALS do
        ;(print chordIntervals)
        (when (equal chordIntervals (second i))
          (setf chordType (first i))
          ;(print chordType)
          (return) ; break loop
         )
       ) ; end loop

  chordType
))
; unit test
(when (not (eq 'maj (getChordType '(0 4 7)))) (error "getChordType:
Major chord"))
(when (not (eq 'min (getChordType '(7 10 2)))) (error "getChordType:
minor chord"))
(when (not (eq 'maj7min (getChordType '(7 11 2 5)))) (error
"getChordType: maj7min chord"))
;(getChordType '(0 7 10 5))
;(getChordType '(5 9 0))


(defun isChord (aList)
 ; (included? (x->dx (list aList)) '(3 4 7)) ; return true if all
intervals found are minor 3rd, major 3rd or 5th
; (and (> (length aList) 1) (included? (x->dx aList) '(3 -9 4 -8 7
-5))) ; return true if all intervals found are minor 3rd, major 3rd
 (getChordType aList) ; if it is one of the known chords it will
return a not nil value
)


; it orders the chord so that a third (or fifth) separes each chord node
; TO-DO this could be done better using a backtracking algorithm
(defmethod! getSortedChord ((self list))
        (remove-if-not #'isChord (all-permutations (remove-duplicates
(mapcar #'(lambda (x) (mod x 12)) self))))
)

;

; (getSortedChord '(48 67 60 52))
; (getSortedChord '(41 65 60 57))
; (getSortedChord '(43 67 62 59))
; (getSortedChord '(48 64 60 55))

;degreeSymbol (I,II,III..)
;chordType (major, minor ...)
;omchord is the original chord from the chord-seq
(defstruct node id key chord degree degreeSymbol tonalFunction
chordType omchord)

; it generates a string to name the node -TO-DO @Quitar?
(defmethod! generateNodeid ((self node) (nlayer integer))
        ;  (format nil "~D-~D-~A-~D_" (node-key self) (node-degree
self) (node-tonalFunction self) (node-chord self))
 (format nil "L~Dk~D~A_d~D_tf~A" nlayer (mod (node-key self) 12) (if
(< (node-key self) 12) 'm 'M) (node-degree self) (node-tonalFunction
self))

)



;TO-DO Urgente: que devuelva varios tipos, p.ej: 0 7 deber@a devolver
tr@ada mayor y menor como posibilidades

; TO-DO Resto de tests unitarios

(defmethod! getNoteNameFromPitchClass ((self integer))
          (if (> self 11) (error "Invalid pitch class, > 11")
          (nth self '(C Db D Eb E F Gb G Ab A Bb B)))
)

; (getNoteNameFromPitchClass 7)

(defmethod! getDegreeSymbol ((degree integer))
          (nth degree '(I II III IV V VI VII))
)

; unit test
(when (not (eq 'I (getDegreeSymbol 0))) (error "getDegreeSymbol I"))

(defun isMajorKey (tonality)
       (> tonality 11)
)

(defun isMinorKey (tonality)
       (< tonality 12)
)



;maj (4 3)) (min (3 4)) (aug (4 4)) (dim (3 3)) (maj7min (4 3 3))
(aug7maj (4 4 3)) (dim7min (3 3 4)) (dim7dim 3 3 3) (maj7maj 4 3 4)
(min7min 3 4 3))
;(defun isAltered (chordType)
;    (position chordType  '(maj7min aug7maj dim7min min7min))
;)

;(defun notIsAltered (chordType)
;    (position chordType  '(maj7min aug7maj dim7min min7min))
;)


;(isAltered 'maj)
;(isAltered 'dim7min)

;TO-DO Hacer con Pl@cido: getPossibleTonalFunctions de Java, para que
no se devuelvan todas las funciones tonales, sino las v@lidas por
acorde
;Para que esto tenga sentido en getChordType debemos admitir m@s tipos
de acordes, p.ej. los de 2 notas
(defmethod! getPossibleTonalFunctions ((degree integer) (chord list)
(chordType symbol) (tonality integer))
        ;  (TONAL_FUNCTIONS '( (T) (S) (S T D) (S) (D T) (S T) (S D)))
                ; (TONAL_FUNCTIONS '( (T) (S) (S T D) (S) (D) (S T) (S D)))

          (if (isMajorKey tonality)
              (  cond
                  ((and (eq degree 0) (eq chordType 'maj)) '(T))
                  ((and (eq degree 1) (member chordType '(min min7min))) '(S))
                  ((and (eq degree 1) (member chordType '(maj
maj7maj))) '(S)) ; neapolitan (and others to be avoided - secondary
dominant - we want it to be handled through other tonality)

                  ((and (eq degree 2) (eq chordType 'min)) '(S T D))
                  ((and (eq degree 2) (member chordType '(min7min))) '(S D))

                  ((and (eq degree 3) (member chordType '(maj maj7maj))) '(S))

                  ((and (eq degree 4) (eq chordType 'maj7min)) '(D))
                  ((and (eq degree 4) (eq chordType 'maj)) '(D T))

                  ((and (eq degree 5) (eq chordType 'min7min)) '(S))
                  ((and (eq degree 5) (eq chordType 'min)) '(S T))

                  ((and (eq degree 6) (member chordType '(dim
dim7min))) '(S D))

                  ;(T (error "Invalid tonal function for major key"))
                  (T '())
                 )
            ;;;; minor key
                (
                 cond
                 ((and (eq degree 0) (member chordType '(min))) '(T))
; TO-DO Igual para el mayor en el último acorde (picarda) poniendo
'maj

                 ((and (eq degree 1) (member chordType '(dim dim7min))) '(S))
                 ((and (eq degree 1) (member chordType '(maj
maj7maj))) '(S)) ; neapolitan (and others to be avoided - secondary
dominant - we want it to be handled through other tonality)

                 ((and (eq degree 2) (member chordType '(aug aug7maj))) '(D))
                 ((and (eq degree 2) (member chordType '(maj7maj))) '(S D))
                 ((and (eq degree 2) (eq chordType 'maj)) '(T S))

                 ((and (eq degree 3) (member chordType '(min min7min))) '(S))

                 ((and (eq degree 4) (eq chordType 'maj7min)) '(D))
                 ((and (eq degree 4) (member chordType '(maj min)))
'(D T)) ; Placido no lo garantiza :(

                 ((and (eq degree 5) (eq chordType 'maj7maj)) '(S))
                 ((and (eq degree 5) (eq chordType 'maj)) '(S T))

                 ((and (eq degree 6) (member chordType '(dim dim7dim))) '(S D))
                 ((and (eq degree 6) (member chordType '(maj maj7min))) '(S))

                 ; (T (error "Invalid tonal function for minor key"))
                 (T '())

                )
            )
)

;(getPossibleTonalFunctions '4 '(5 9 0) 'maj 23)
;(setq scale '((0) (1 2) (3 4) (5) (7) (8 9) (10 11)))
;(position '11 scale :test 'position)


; TO-DO Que esto devuelva una lista de pares tonalidad x acorde
; Esto lo coger@ la funci@n que a@adir@ posibles funciones tonales
; Quiz@s valga la pena generar para todos lo acordes las tres posibles
funciones tonales
; IMPORTANTE: deber@amos recibir o enviar los acordes ya ordenados por
terceras para establecer el grado
(defmethod! generateGraphNodes ((self chord-seq))
          ;(setf workTonality '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
16 17 18 19 20 21 22 23)) ; only for testing
          ;(print (getValidKeys '(60 62 64 65 67 69 71)))
          (let* ((MAJOR_SCALE '((0) (2) (4) (5) (7) (9) (11)))
                 (MINOR_NATURAL_SCALE '((0) (1 2) (3 4) (5) (7) (8 9) (10 11)))
                 (nlayer 0)
                 (nnode 1)
                 layers
                 )
            ; add first an artificial layer with a single node as the
starting point from the multilayer graph
            (push (list (make-node :id 0)) layers)

            (loop for omchord in (lmidic self) do
                  ;(print "New chord")
                  ; for each chord get all valid tonalities
                  (let* ((chordNotes (om/ omchord 100))
                        (validKeys (getValidKeys chordNotes)) ; send
the midi pitches
                  ; (print validKeys)
                 ; (push (getSortedChord chordNotes) chords)
                        (chords (getSortedChord chordNotes))
                        (layer '()))
                   ; (print chords)
                    ;(print chordNotes)
                  (loop for key in validKeys do
                        (loop for chord in chords do
                              (let* ((root (first chord))
                                     (keyRoot (mod key 12))
                                     (scale (if (< key 12)
MINOR_NATURAL_SCALE MAJOR_SCALE))
                                     (stepsOfRootFromKeyRoot (mod (-
(+ 12 root) keyRoot) 12)); sum of 12 to avoid negative numbers
                                     (degree (position
stepsOfRootFromKeyRoot scale :test 'position))
                                     validTonalFunctions node)

                              (when degree ; if degree is not null -
20090218: no puede ser null!!!
                                ;(setf validTonalFunctions (nth
degree TONAL_FUNCTIONS))
                                (setf validTonalFunctions
(getPossibleTonalFunctions degree chord (getChordType chord) key))
                                (loop for tonalFunction in
validTonalFunctions do
                                      (setf node (make-node :key key
                                                            :chord chord
                                                            :degree degree
                                                            :omchord omchord

:tonalFunction tonalFunction
                                                            ))
                                      ;(setf (node-id node)
(generateNodeid node nlayer))
                                      (setf (node-id node) nnode)
                                      (setf (node-degreeSymbol node)
(getDegreeSymbol degree))
                                      (setf (node-chordType node)
(getChordType chord)) ; URGENT Esto lo podemos recibir como par@metro,
lo tenemos ya calculado en la l@nea antes del loop
                                      ;(print (node-chord node))
                                      ;(print (node-chordType node))
                                      (setq nnode (+ nnode 1))
                                      (push node layer)
                                    ) ; end loop
                              ) ; end when
                        ))
                  )
                  (push (reverse layer) layers) ; reverse because we
use a stack
                  ;(print (cartesian (chords validKeys)))
                  ;(setf workTonality (x-intersect workTonality validKeys))
                  ;(print workTonality)
                  (setq nlayer (+ nlayer 1))
            )) ; end loop
           ; (print chords)
           ; (print "Work tonality: " workTonality)
           (reverse layers) ; because we have use a stack
))

(defmethod! keyToString ((self integer))
          (format nil "~D ~A ~D" (getNoteNameFromPitchClass (mod self
12)) (if (< self 12) 'Min 'Maj) self)
)



(defmethod! keyToStringWithouSpaces ((self integer))
          (format nil "~D~A" (getNoteNameFromPitchClass (mod self
12)) (if (< self 12) 'Min 'Maj))
)

(defmethod! getNoteAndAltFromPitchClass ((self integer))
          (nth self '((do becarre) (re bemol) (re becarre) (mi bemol)
(mi becarre) (fa becarre) (sol bemol) (sol becarre) (la bemol) (la
becarre) (si bemol) (si becarre)))
)


;(loop for i from 0 to 23 do  (print (getNoteAndAltFromPitchClass i)))

; TO-DO Unit test
;(keyToString 0)
(keyToString 17)

(defmethod! keyToTonalite ((self integer))
          ;(print "Hola")
          ;(print self)
          (make-instance 'tonalite :mode (if (< self 12) *mineur*
*majeur*) :tonnote (first (getNoteAndAltFromPitchClass (mod self 12)))
:alt  (second (getNoteAndAltFromPitchClass (mod self 12))) )
)

;(tonnote (keyToTonalite 7))


; returns nil when no edge inputs to this node and no edge outputs from it
(defmethod isConnectedNode ((self node)  (adjacencyMatrix simple-array))
 (let* ((id (node-id self))
       (n (first (array-dimensions adjacencyMatrix)))
       connected
       )
 ; (print n)
  (loop for i from 0 to (- n 1) do
        (when (aref adjacencyMatrix i id)
               (setf connected t)
               (return))
        (when (aref adjacencyMatrix id i)
               (setf connected t)
               (return))
  )
  connected
))

; true if the node is the artificial starting node
(defmethod! isStartingNode ((self node))
          (eq (node-id self) 0)
)

;(isStartingNode(make-node :id 0))

(defmethod! analysisToString ((self node))
          (format nil "(~A ~A ~A ~A) " (keyToStringWithouSpaces
(node-key self)) (node-degreeSymbol self) (node-chordType self)
(node-tonalFunction self) )
)



; write the graph in a file for debug purposes
(defmethod! writeDotFile ((self list) (adjacencyMatrix simple-array)
(bestPath list))
          (with-open-file (stream "/tmp/ensayo.dot" :direction
:output :if-exists :supersede :if-does-not-exist :create)
            (with-open-file (streamo "/tmp/oncina.txt" :direction
:output :if-exists :supersede :if-does-not-exist :create)
            (let* ((nlayer 0)
                   prevLayer
                   printedChords
                   ourAnalysis
                   )
            (format stream "digraph g {~%compound=true;~%center=true;~%")
            (loop for layer in self do

                 (format stream "subgraph cluster~D {~%label=\"Layer
~D\";~%" nlayer nlayer)
                 ; (format stream "{~%" )
                 ; (format stream "%rank = same;~%" nlayer)
                 ;(format stream "~D;~%" nlayer)
                  (setf printedChords nil)
                  (setf nlayer (+ nlayer 1))
                  (when (equal (length layer) 0) (format stream "el~D
[label=\"                 No found chords in layer ~D
\"];~%" nlayer nlayer))

                  (setf ourAnalysis nil)
                  (loop for node in layer do
                        (when (not (or (isStartingNode node)
(included? (list (node-chord node)) printedChords)))
                         ; (print (node-id node))
                         ; (print bestPath)
                         ; (print (member (node-id node) bestPath))
                          (format stream "c~D [label=\"L~D Chord
~A\"];~%" (node-id node) nlayer (node-chord node))
                          (push (node-chord node) printedChords)
                        (format streamo "Chord ~D:~A~%" nlayer
(node-chord node))
                        (format streamo "possibilities: ")
                        )
                        (if (isStartingNode node)
                            (format stream "n0 [label=\"\"];~%")
                          (when (isConnectedNode node adjacencyMatrix)
                            (format stream "n~D [label=\"~D k=~A ~A~A
tf=~A\" ~A];~%" (node-id node) (node-id node) (keyToString (node-key
node)) (node-degreeSymbol node) (node-chordType node)
(node-tonalFunction node) (if (member (node-id node) bestPath)
",style=filled" "") )
                            ;(format streamo "(k=~A ~A~A tf=~A)"
(keyToStringWithoutSpaces (node-key node)) (node-degreeSymbol node)
(node-chordType node) (node-tonalFunction node) )
                            (format streamo "~A" (analysisToString node))
                   (when (member (node-id node) bestPath) (setf
ourAnalysis (analysisToString node))
                          )
                            )

                          ;(format streamo "(k=~A ~A~A tf=~A\" ~A)"
(keyToString (node-key node)) (node-degreeSymbol node) (node-chordType
node) (node-tonalFunction node) )


                         )

                  ); end loop nodes of layer
                  (when (not (NULL ourAnalysis))
                    (format streamo "~%") ; EOL
                      (format streamo "expected: ~A~%" ourAnalysis) ; TO-DO
                    (format streamo "ouranalysis: ~A~%" ourAnalysis)
                    )
                ;  (when prevLayer ; != nil
                 ;   (loop for fromNode in prevLayer do
                  ;        (loop for toNode in layer do
                   ;             (let* ((inode (node-id fromNode))
                    ;                   (jnode (node-id toNode))
                     ;                  (weight (first (aref
adjacencyMatrix inode jnode)))
                      ;                 )
                       ;           (when weight
                        ;            (format stream "n~D->n~D
[label=\"~D\"];~%" (node-id fromNode) (node-id toNode) weight)
                         ;         ))
                          ;      )
                          ;)
                  ;)

                  (setf prevLayer layer)
                  (format stream "cluster~D;}~%" (- nlayer 1))  ; it
was already incremented
            ); end loop layers
            (loop for nl from 0 to (- nlayer 2) do
             (format stream "cluster~D->cluster~D;~%" nl (+ nl 1))
            )

            ; print edges
            (loop for i from 0 to (- (first (array-dimensions
adjacencyMatrix)) 1) do
                  (loop for j from 0 to (- (second (array-dimensions
adjacencyMatrix)) 1) do
              (let* ((weight (first (aref adjacencyMatrix i j))))
                (when (and weight (not (equal weight KEY_CHANGE_WEIGHT)))
                  (format stream "n~D->n~D [label=\"~D\"];~%" i j weight)
              ))
            ))
            (format stream "}~%")
            )
          )
            )
)

(defmethod! getChordDesc ((self node))
          (format nil "~A~A" (node-degreeSymbol self) (node-chordType self))
)

;(getChordDesc (make-node :degreeSymbol 'I :chordType 'maj))
;(getChordDesc (make-node :degreeSymbol 'v :chordType 'maj7min))

; fromChordDesc must have the form (chordDegreeSymbol chordType), e.g (V major)
;(defmethod! isCadence ((fromNode node) (toNode node) (fromChordDesc
list) (toChordDesc list))
;            (and (equal (getChordDesc fromNode) fromChordDesc) (equal
(getChordDesc toNode) toChordDesc))
;)

(setq KEY_CHANGE_WEIGHT '-1000)

; TO-DO Pesos
(defmethod! computeWeight ((fromNode node) (toNode node))
          ;(print fromNode)
          ;(print toNode)
          (print (getChordDesc toNode))
 (if (isStartingNode fromNode) '(0)        ; the edges from node 0
(artificial beginning node) are weight 0
 (if (eq (node-key fromNode) (node-key toNode))
 (cond
 ((equal (node-degree fromNode) (node-degree toNode)) '(1))
 ((and (string-equal (getChordDesc fromNode) "vmaj7min") (position
(getChordDesc toNode) '("imaj" "imin") :test #'string-equal)) '(2500))
 ((and (string-equal (getChordDesc fromNode) "vmaj7min") (position
(getChordDesc toNode) '("vimaj" "vimin") :test #'string-equal))
'(2100))
 ((and (string-equal (getChordDesc fromNode) "vmaj") (position
(getChordDesc toNode) '("imaj" "imin") :test #'string-equal)) '(1900))
 ((and (string-equal (getChordDesc fromNode) "viidim7min")
(string-equal (getChordDesc toNode) "imaj")) '(2300))
 ((and (string-equal (getChordDesc fromNode) "viidim7dim")
(string-equal (getChordDesc toNode) "imin")) '(2300))
 ((and (string-equal (getChordDesc fromNode) "viidim") (position
(getChordDesc toNode) '("imaj" "imin") :test #'string-equal)) '(1600))
 ((and (string-equal (getChordDesc fromNode) "iiimaj7maj")
(string-equal (getChordDesc toNode) "imin")) '(1550))
 ((and (string-equal (getChordDesc fromNode) "iiimaj7min")
(string-equal (getChordDesc toNode) "imaj")) '(1575)) ; nuevo 20090217
 ((and (string-equal (getChordDesc fromNode) "iiimin") (string-equal
(getChordDesc toNode) "imaj")) '(1535)) ; nuevo 20090217
 ((and (string-equal (getChordDesc fromNode) "iiiaug7maj")
(string-equal (getChordDesc toNode) "imin")) '(1575))
 ((and (string-equal (getChordDesc fromNode) "iiimaj") (string-equal
(getChordDesc toNode) "imaj")) '(1500))
 ((and (string-equal (getChordDesc fromNode) "iiiaug") (string-equal
(getChordDesc toNode) "imin")) '(1500))
 ((and (equal (node-tonalFunction fromNode) 'T) (eq
(node-tonalFunction toNode) 'D)) '(26))
 ((and (equal (node-tonalFunction fromNode) 'T) (eq
(node-tonalFunction toNode) 'S)) '(75))
 ((and (equal (node-tonalFunction fromNode) 'T) (eq
(node-tonalFunction toNode) 'T)) '(1))
 ((and (equal (node-tonalFunction fromNode) 'S) (eq
(node-tonalFunction toNode) 'D)) '(100))
 ((and (equal (node-tonalFunction fromNode) 'S) (eq
(node-tonalFunction toNode) 'T)) '(145))
 ((and (equal (node-tonalFunction fromNode) 'S) (eq
(node-tonalFunction toNode) 'S)) '(1))
 ((and (equal (node-tonalFunction fromNode) 'D) (eq
(node-tonalFunction toNode) 'S)) '(-101))
 ((and (equal (node-tonalFunction fromNode) 'D) (eq
(node-tonalFunction toNode) 'D)) '(1))
 ((and (equal (node-tonalFunction fromNode) 'D) (eq
(node-tonalFunction toNode) 'T)) '(250))
 (t nil) ; else, when not a valid progression is found
 ) (list KEY_CHANGE_WEIGHT)))
)
; (eq (getChordDesc #S(node :id 39 :key 12 :chord (7 11 2 5) :degree 4
:degreesymbol v :tonalfunction t :chordtype maj7min)) "vmaj7min")
; (computeWeight #S(node :id 39 :key 12 :chord (7 11 2 5) :degree 4
:degreesymbol v :tonalfunction t :chordtype maj7min) #S(node :id 52
:key 12 :chord (0 4 7) :degree 0 :degreesymbol i :tonalfunction t
:chordtype maj))

(getChordDesc #S(node :id 39 :key 12 :chord (7 11 2 5) :degree 4
:degreesymbol v :tonalfunction d :chordtype maj))
(getChordDesc #S(node :id 39 :key 12 :chord (7 11 2 5) :degree 4
:degreesymbol i :tonalfunction t :chordtype maj))

(eq (getChordDesc #S(node :id 39 :key 12 :chord (7 11 2 5) :degree 4
:degreesymbol v :tonalfunction t :chordtype maj7min)) "vmaj7min")
; (node-tonalFunction (make-node))
 (computeWeight #S(node :id 39 :key 12 :chord (7 11 2 5) :degree 4
:degreesymbol v :tonalfunction d :chordtype maj) #S(node :id 52 :key
12 :chord (0 4 7) :degree 0 :degreesymbol i :tonalfunction t
:chordtype maj))


;TO-DO Ahora llamamos a generateEdges desde writeDotFile, luego habr@
que sacar el mejor camino
; generate edges from layer to layer
; it outputs a list of edges with the form (id-node-from id-node-to weight)
(defmethod! generateEdgesBetweenLayers ((fromLayer list) (toLayer list))
 (setq edges '())
 (loop for fromNode in fromLayer do
      (loop for toNode in toLayer do
            ;(setq weight 1)
            (setq weight (computeWeight fromNode toNode))
            (push (list (node-id fromNode) (node-id toNode) weight) edges)
      )
 )
 (reverse edges)
)


(defmethod! generateEdges ((self list))
 (let* ((nnodes (length (flat self))) ; TO-DO Hacerlo mejor
       prevLayer
       nonEmptyLayers ; layers removing those that contain no chord
       (adjacencyMatrix (make-array (list nnodes nnodes)))) ; default
value is nil

  (loop for layer in self do
        (when layer (push layer nonEmptyLayers)
        )
        )

 (loop for layer in (reverse nonEmptyLayers) do
      (when prevLayer ; != nil
        (loop for edge in (generateEdgesBetweenLayers prevLayer layer) do
              (setf (aref adjacencyMatrix (first edge) (second edge))
(third edge))
              ;(print (first edge))
              ;(print (aref adjacencyMatrix (first edge) (second edge)))
        )
      ) ; end when
      (setf prevLayer layer)
 )
 adjacencyMatrix
))
;(type-of (make-array '(2 2)))

; http://www.lsi.upc.es/~iea/transpas/4_dinamica/sld017.htm
(defmethod! computeBestPathMultilayerGraph ((self list)
(adjacencyMatrix simple-array))
 (let* ((n (first (array-dimensions adjacencyMatrix)))
       (C (make-array n :initial-element 0))  ; dynamic programming
matrix, init to the number of layers
       (D (make-array n :initial-element 0))
       path
       (nlayers (length self)))
       ;(vpath (make-array (+ 1 nlayers) :initial-element 0))
       (loop for j from  (- n 1) downto 0 do
             ; get the best source vertex
             (let* ((maxWeight)
                   (imaxWeight)
                   (tmpsum)
                   result
                   )
                   ;(print C)
                   (loop for r from 0 to (- n 1) do
                         (when (aref adjacencyMatrix j r) ; if there are edge
                           (setf tmpsum (+ (first (aref
adjacencyMatrix j r)) (aref C r)))
                           (when (or (not maxWeight) (> tmpsum
maxWeight)) ; when > or maxWeight was nil
                             (setf maxWeight tmpsum)
                             (setf imaxWeight r)
                             )
                           )
                   )
                   (when maxWeight
                     (setf (aref C j) maxWeight))
                   (when imaxWeight
                     (setf (aref D j) imaxWeight))
             )
       )
       ; build the path

       ;(setf (aref path 0) 0)
       (push 0 path)
       ;(setf (aref path nlayers) n)
       (loop for j from 1 to (- nlayers 1) do
             ;(setf (aref vpath j) (aref D (aref vpath (- j 1))))
             (push (aref D (first path)) path)
             )

;(print "------------------")
;(print path)
;(print (reverse path))
;(print "..........")
 (reverse path)
))

; return the sequence of best path nodes. nil is used whenever the
layer does not contain any best path node
(defmethod! bestPathIdsToNodes ((self list) (path list))
          (let* ((bestPathNodes))
       ; now put nil whenever a layer does not contain any node
       (loop for layer in self do
             ; look for the node that is contained in the best path
             (let* ((nodeInBestPath nil)
                    (found nil))
               (loop for node in layer do
                   (when (member (node-id node) path)
                     (push node bestPathNodes)
                     (setf found t)
                      (return)) ; found, break loop
               )
               (when (not found) (push nil bestPathNodes))

             ))

     ;  (print bestPathNodes)
 (cdr (reverse bestPathNodes)) ; remove the first artificial node
))

; (computeBestPathMultilayerGraph '(0 2) (make-array '(2 2) :initial-element 0))

(defmethod! isNodeID ((self node) (n integer))
        ;  (print self)
        ;  (print n)
        (equal (node-id self) n)

)

(defmethod! findNode ((nodes list) (id integer))
          (let* (result)
      ;      (print "...............................")
          (loop for inode in nodes do
       ;         (print inode)
        ;        (print id)
                (when (isNodeID inode id)
                  (setf result inode))
                )
          result)
)

(defmethod! getNodeLyrics ((self node))
          (list
           (format nil "k=~A" (keyToString (node-key self)))
           (format nil "~A~A" (string (node-degreeSymbol self))
(node-chordType self))
           (format nil "tf=~A" (node-tonalFunction self) ))
)


(defmethod! bestPathToTonalitiies ((bestPathNodes list))
          (let* ((tonalities)
                 prevTonalite)
                 (loop for node in bestPathNodes do
                       (if node
                           (let* ((ton (keyToTonalite (node-key node))))
                             (push ton tonalities)
                             (setf prevTonality ton)
                          )
                         ; else
                         (push prevTonalite tonalities))
                 ) ; end loop layers

                 (reverse tonalities)

))


(defmethod! bestPathToText ((bestPathNodes list))
          ;(print "entra"
          (let* ((lyrics))

                 (loop for node in bestPathNodes do
                       (if node
                           (push (getNodeLyrics node) lyrics)
                           (push '("-" "-" "-") lyrics))

                 ) ; end loop layers
                 ;(print lyrics)
                 (reverse lyrics)

))

(defmethod! nodeToItemCaption ((self node))
          (format nil "k=~A, ~A~A, tf=~A" (keyToString (node-key
self)) (string (node-degreeSymbol self)) (node-chordType self)
(node-tonalFunction self))
)


(defmethod! buildGraph ((self chord-seq))
          :numouts 3
 (let* ((nodes (generateGraphNodes self))
      (adjacencyMatrix (generateEdges nodes))
      outedges
      outnodes
      bestPathNodeCircle
      annotationsList
      m
      bestpathArcs
      nodesPerLayers
      onsets
      (cont 0)
      )
          ;(print adjacencyMatrix) ; TO-DO Me lo deja todo a nil
       ;   (loop for i from 0 to 500 do
       ;     (loop for j from 0 to 500 do
       ;       (print (aref adjacencyMatrix i j))
       ;       )
       ;     )

          (setf bestPath (computeBestPathMultilayerGraph nodes
adjacencyMatrix))
          (setf bestPathNodes (bestPathIdsToNodes nodes bestPath))
          (writeDotFile nodes adjacencyMatrix bestPath)
          ;nodes
         ; (print "Best path nodes: ")
         ; (print bestPathNodes)


          ; put lyrics
          (loop for element in (bestPathToText bestPathNodes)
                for chord in (inside self) do
                (push (list (offset->ms chord)  element) annotationsList))
               ; (loop for str in element
                ;      for i = 2 then (+ i 2) do
                 ;     (add-extra chord (make-instance 'text-extra
:deltay i :thetext str) nil)) )

          ; put tonalities
          ; build the output
          (let* ((bestPathTonalities (bestPathToTonalitiies bestPathNodes))

                 )
           ; (print bestPathTonalities)
            (loop for element in bestPathTonalities
                for chord in (inside self) do
                (set-tonalite chord element))


          ; put score tonality as the first one
           ; (set-tonalite self (first bestPathTonalities))
           (loop for node in bestPathNodes
                 for layer in (cdr nodes) ; remove the first empty node
                 for chord in (inside self) do
                 ;(print (node-chord node))
                 (when node
                     (push (make-instance 'node-cercle :linenum (mod
cont 2) :colnum cont :obj (make-instance 'n-cercle :n 12 :puntos
(node-chord node))) bestPathNodeCircle)
                     (print cont)
                     (let* ((layerCircleNodes))
                       (loop for layerNode in layer do
                             ;(print layerNode)
                             (push (nodeToItemCaption layerNode)
layerCircleNodes)
                           )

                       (push layerCircleNodes nodesPerLayers)
                       )


                     (push (offset->ms chord) onsets)
                     (when (> cont 0)
                       (push (make-instance 'standard-arc  :from-node
(list (mod (- cont 1) 2) (- cont 1 )) :to-node (list (mod cont 2)
cont)) bestpathArcs)
                             )
                     (setf cont (+ cont 1))

                 ))
          ; (print
          ; create the output matrix graph
          (setf m (make-instance 'matrix-graph-tonality :numlines 1
:numcols (length annotationsList) :arcs (reverse bestpathArcs) :nodes
(reverse bestPathNodeCircle) :columninfo (reverse nodesPerLayers)
:col-pos (reverse onsets) :nodes nodes :adjacencyMatrix
adjacencyMatrix))
          (print (length bestPathNodeCircle))
          (print (length annotationsList))
          )
           ; list of nodes
          (loop for layer in nodes do
                )

           ; list of edges
          (loop for i from 0 to (- (first (array-dimensions
adjacencyMatrix)) 1) do
                  (loop for j from 0 to (- (second (array-dimensions
adjacencyMatrix)) 1) do
              (let* ((weight (first (aref adjacencyMatrix i j))))
                (when (and weight (not (equal weight KEY_CHANGE_WEIGHT)))
                  (push (list i j weight) outedges)
              ))
            ))

          (values self (make-instance 'temporal-text :textos (reverse
annotationsList)) m)
         ; (list (reverse outnodes) (reverse outedges) (cdr
bestPath)) ; for the best path the first node (the artificial one) is
removed
))


;(make-instance 'text-extra :deltay 2 :thetext "str")

;(timeSignaturesToStrongBeats '((4 4) (4 4) (4 4) (4 4) (4 4) (4 4) (44) (4 4) (4 4)))