;
; Babby's First Lisp Program
;
; TODO:
;  * Store the player's guesses and implement a lose option, as right
;    now, it's just "guess-the-word" instead of hangman
;  * Think harder and store the word to guess and the status as a local
;    variable
;  * Improve indenting style (?)
;  * Find out how to clear the terminal (dirty hack: print 24 new lines)
;  * Implement a repeat-game loop and prompt the user when to quit
;

(setf *random-state* (make-random-state t))

(defvar *wordlist* '("CAT" "MOUSE" "DOG" "ONOMATOPOEIA" "ANACOLUTHON" "RESPITE"
                     "PARALIPSIS" "PSEUDOLOGY" "POTVALIANCY" "FASTIGIATE"
                     "EXORDIUM" "GASCONADE" "SOLECISM" "PROFLIGACY" "PERSNICKETY"
                     "HETEROTELIC" "IDONEOUS" "REMORA" "MULTIPLICATION"
                     "INCARNADINE" "REDOLENT" "INNOCUOUS" "CONVIVIAL" "TENEBROUS"
                     "AESTHETE" "TRISKAIDEKAPHOBIA"))

; function to get a random word from the global *wordlist*
(defun random-word (wordlist)
  (nth (random (length wordlist)) wordlist))

; function that checks if a user's letter guess is within the word
; (word-check "F" "DOG") -> (F F F)
; (word-check "B" "BOB") -> (T F T)
(defun word-check (guess word)
  (map 'list
    (lambda (letter) (string-equal letter guess))
    word))

(defun update-num-guesses (letter string num-guesses)
  (if (find letter string :test #'string-equal)
      num-guesses
      (- num-guesses 1)))

; function to print the unknown word in the style of hangman
; (print-word '(F T F T T) "MOUSE" ) -> "_O_SE"
(defun print-word (status word)
  (let ((constructed-word
          (map 'string
            (lambda (x y) (if (null x) #\_ y))
            status word)))
    (format t "Word: ~a ~%" constructed-word)))

; function that checks if the game should be stopped
; checks if all the members of the status list are T
(defun hangman-wonp (status)
  (every #'identity status))

; print winning message and word
(defun won-game (word)
  (format t "You win! ~%")
  (format t "The word was ~a ~%" word))

(defun lose-game (word)
  (format t "You lose! ~%")
  (format t "The word was ~a ~%" word))

; function to update the status
(defun update-status (old-status new-status)
  (mapcar (lambda (x y) (or x y)) old-status new-status))

(defun game-loop (word old-status num-guesses)
  (if (= num-guesses 0)
      (lose-game word)
      (progn
        (print-word old-status word)
        (format t "Number of guesses left: ~a ~%" num-guesses)
        (format t "Enter a letter: ~%")
        (let* ((guessed-letter (read))
               (entered-status (word-check guessed-letter word))
               (new-status (update-status old-status entered-status))
               (new-num-guesses (update-num-guesses guessed-letter word num-guesses)))
          (cond ((hangman-wonp new-status) (won-game word))
                (t (game-loop word new-status new-num-guesses)))))))

; game main loop
(defun main-game ()
  (let* ((guess-word (random-word *wordlist*))
         (status (word-check " " guess-word)))
    (game-loop guess-word status 5)))

(main-game)

