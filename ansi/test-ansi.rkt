#lang racket/base

(require "ansi.rkt")

(for-each display (list (dec-soft-terminal-reset)

			(select-graphic-rendition style-bold
						  (style-text-color color-yellow)
						  (style-background-color color-blue))
			(clear-screen/home)
			(dec-double-width-single-height)
			"Hello world!"
			(move-cursor-left 6)
			(insert-characters 5)
			"ANSI"
			"\n"
			(dec-double-width-double-height-top)
			"Bigger yet\n"
			(dec-double-width-double-height-bottom)
			"Bigger yet\n"
			(dec-single-width-single-height)
			"Normal\n"
			(move-cursor-up 3)
			(select-graphic-rendition style-normal-intensity)
			(select-graphic-rendition (style-text-color color-white)
						  (style-background-color color-red))
			(insert-lines 3)
			"Test\n"
			(select-graphic-rendition (style-text-color color-white)
						  (style-background-color color-green))
			(delete-lines 2)
			(move-cursor-up 1)
			(goto-column 2)
			(delete-characters 1)
			(select-graphic-rendition)

			(goto 19 1)))

