;
; Babby's First Lisp Program
;
; TODO:
;  * Improve indenting style (?)
;  * Find out how to clear the terminal (dirty hack: print 24 new lines)
;  * Store and check for repeated guesses
;

(setf *random-state* (make-random-state t))

(defvar *wordlist* '("CAT" "MOUSE" "DOG" "ONOMATOPOEIA" "ANACOLUTHON" "RESPITE"
                     "PARALIPSIS" "PSEUDOLOGY" "POTVALIANCY" "FASTIGIATE"
                     "EXORDIUM" "GASCONADE" "SOLECISM" "PROFLIGACY" "PERSNICKETY"
                     "HETEROTELIC" "IDONEOUS" "REMORA" "MULTIPLICATION"
                     "INCARNADINE" "REDOLENT" "INNOCUOUS" "CONVIVIAL" "TENEBROUS"
                     "AESTHETE" "TRISKAIDEKAPHOBIA"))

; get a random word from the specified wordlist
(defun random-word (wordlist)
  (nth (random (length wordlist)) wordlist))

; checks if a user's letter guess is within the word
; (word-check "B" "BOB") -> (T F T)
(defun word-check (guess word)
  (map 'list
    (lambda (letter) (string-equal letter guess))
    word))

; if a character is in the string, don't decrease the guess count
(defun update-num-guesses (letter string num-guesses)
  (if (find letter string :test #'string-equal)
    num-guesses
    (- num-guesses 1)))

; print the unknown word in the style of hangman
; (print-word '(F T F T T) "MOUSE" ) -> "_O_SE"
(defun print-word (status word)
  (let ((constructed-word
          (map 'string
            (lambda (predicate letter) (if (null predicate) #\_ letter))
            status word)))
    (format t "Word: ~a~%" constructed-word)))

; checks if all the members of the status list are T
; and therefore if the game should be stopped
(defun hangman-wonp (status)
  (every #'identity status))

(defun won-game (word)
  (format t "You win!~%")
  (format t "The word was ~a~%" word))

(defun lose-game (word)
  (format t "You lose!~%")
  (format t "The word was ~a~%" word))

; update the status by OR'ing the old and new status
(defun update-status (old-status new-status)
  (mapcar (lambda (x y) (or x y)) old-status new-status))

(defun game-loop (word old-status num-guesses)
  (if (= num-guesses 0)
      (lose-game word)
      (progn
        (print-word old-status word)
        (format t "Number of guesses left: ~a~%" num-guesses)
        (format t "Enter a letter:~%")
        (let* ((guessed-letter (read))
               (entered-status (word-check guessed-letter word))
               (new-status (update-status old-status entered-status))
               (new-num-guesses (update-num-guesses guessed-letter word num-guesses)))
          (if (hangman-wonp new-status) (won-game word)
            (game-loop word new-status new-num-guesses))))))

; game main loop
(defun main-game ()
  (let* ((guess-word (random-word *wordlist*))
         (status (word-check " " guess-word)))
    (format t "Your word has ~a letters.~%" (length guess-word))
    (game-loop guess-word status 5))
  (format t "Do you wish to play again? [y/yes/N]~%")
  (let* ((again (read)))
    (if (or (string-equal "Y" again)
            (string-equal "YES" again)) (main-game))))

;(main-game)
