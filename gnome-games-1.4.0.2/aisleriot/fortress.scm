; AisleRiot - fortress.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
;
; This game is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
; USA

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-extended-slot '() right)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() right)

  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot DECK)
  (add-blank-slot)
  (add-extended-slot '() right)

  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-extended-slot '() right)

  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-extended-slot '() right)
  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-extended-slot '() right)

  (deal-cards-face-up 3 '(0 2 5 8 11 1 4 7 10 13 0 2 5 8 11 1 4 7 10 13 0 2 5 8 11 1 4 7 10 13 0 2 5 8 11 1 4 7 10 13 0 2 5 8 11 1 4 7 10 13 0 1))

  (list 10 5))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (not (= slot-id 3))
       (not (= slot-id 6))
       (not (= slot-id 9))
       (not (= slot-id 12))
       (= (length card-list) 1)))

(define (button-released start-slot card-list end-slot)
  (cond ((or (= end-slot 3)
	     (= end-slot 6)
	     (= end-slot 9)
	     (= end-slot 12))
	 (and (or (and (empty-slot? end-slot)
		       (= (get-value (car card-list)) ace))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (get-top-card end-slot))
			  (get-suit (car card-list)))
		       (= (get-value (car card-list))
			  (+ 1 (get-value (get-top-card end-slot))))))
	      (move-n-cards! start-slot end-slot card-list)
	      (add-to-score! 1)))
	((or (empty-slot? end-slot)
	     (and (= (get-suit (get-top-card end-slot))
		     (get-suit (car card-list)))
		  (or (= (get-value (get-top-card end-slot))
			 (+ 1 (get-value (car card-list))))
		      (= (+ 1 (get-value (get-top-card end-slot)))
			 (get-value (car card-list))))))
	 (move-n-cards! start-slot end-slot card-list))
	(#t #f)))

(define (button-clicked slot-id)
  #f)

(define (move-to-foundation slot f-slot)
  (cond ((= f-slot 15)
	 #f)
	((and (empty-slot? f-slot)
	      (= (get-value (get-top-card slot)) ace))
	 (and (deal-cards slot (list f-slot))
	      (add-to-score! 1)))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card f-slot))
		 (get-suit (get-top-card slot)))
	      (= (+ 1 (get-value (get-top-card f-slot)))
		 (get-value (get-top-card slot))))
	 (and (deal-cards slot (list f-slot))
	      (add-to-score! 1)))
	(#t (move-to-foundation slot (+ 3 f-slot)))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 0)
	   (not (= (modulo slot-id 3) 0)))
       (move-to-foundation slot-id 3)))

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 2)
       (empty-slot? 4)
       (empty-slot? 5)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 10)
       (empty-slot? 11)
       (empty-slot? 13)))

(define (to-foundations? slot f-slot)
  (cond ((= slot 14)
	 #f)
	((or (empty-slot? slot)
	     (= slot 3)
	     (= slot 6)
	     (= slot 9)
	     (= slot 12)
	     (= f-slot 15))
	 (to-foundations? (+ 1 slot) 3))
	((and (empty-slot? f-slot)
	      (= (get-value (get-top-card slot))
		 ace))
	 (list 2
	       (get-name (get-top-card slot))
	       "an empty Foundation"))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card f-slot))
		 (get-suit (get-top-card slot)))
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (list 1
	       (get-name (get-top-card slot))
	       (get-name (get-top-card f-slot))))
	(#t (to-foundations? slot (+ 3 f-slot)))))

(define (to-tableau? slot1 slot2)
  (cond ((= slot1 14)
	 #f)
	((or (empty-slot? slot1)
	     (= slot2 14)
	     (= slot1 3)
	     (= slot1 6)
	     (= slot1 9)
	     (= slot1 12))
	 (to-tableau? (+ 1 slot1) (+ 2 slot1)))
	((and (not (or (= slot2 3)
		       (= slot2 6)
		       (= slot2 9)
		       (= slot2 12)))
	      (not (empty-slot? slot2))
	      (= (get-suit (get-top-card slot1))
		 (get-suit (get-top-card slot2)))
	      (or (= (get-value (get-top-card slot1))
		     (+ 1 (get-value (get-top-card slot2))))
		  (= (get-value (get-top-card slot2))
		     (+ 1 (get-value (get-top-card slot1))))))
	 (list 1
	       (get-name (get-top-card slot1))
	       (get-name (get-top-card slot2))))
	(#t
	 (to-tableau? slot1 (+ 1 slot2)))))

(define (empties?)
  (and (or (empty-slot? 0)
	   (empty-slot? 1)
	   (empty-slot? 2)
	   (empty-slot? 4)
	   (empty-slot? 5)
	   (empty-slot? 7)
	   (empty-slot? 8)
	   (empty-slot? 10)
	   (empty-slot? 11)
	   (empty-slot? 13))
       (list 0 "Move something into the empty Tableau slot")))

(define (get-hint)
  (or (to-foundations? 0 3)
      (to-tableau? 0 1)
      (empties?)))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
