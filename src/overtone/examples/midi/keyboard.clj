(ns overtone.examples.midi.keyboard
  (:use overtone.live)
  (:require [overtone.inst.synth :as synth]
            [overtone.synth
             [retro :as retro]
             [sts :as sts]]
            [overtone.examples.synthesis.fm :as fm]
            [overtone.studio.midi-player :as mp]
            [overtone.examples.compositions.bells :as bells]))

(definst ding
  [note 60 velocity 100]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (perc 0.1 0.8) :action FREE)]
    (* velocity env snd)))

(defn midi-player [event]
  (ding (:note event) (/ (:velocity event) 127.0)))

; Calling midi-in without any arguments will bring up a swing dialog to choose
; from available midi devices.  Note that Java does not seem to detect midi devices
; after JVM startup (at least on OSX), so you USB midi device will need to be connected
; before starting Overtone.

;(def keyboard (midi-in))

; The low level midi handler mechanism from midi-clj uses midi-handle-events,
; which takes a device ; and a midi player function that will receive midi
; event maps.

;(midi-handle-events keyboard #'midi-player)

; overtone.studio.midi now includes the beginnings of a higher level midi interface
; that helps improve on this.  By default Overtone will detect and listen to all
; midi input devices.  These midi messages are then sent as events, which can be received
; with the Overtone event system.

; For example, print out all incoming note-on messages:

;(on-event [:midi :note-on] (fn [{note :note velocity :velocity}]
;                             (println "Note: " note ", Velocity: " velocity))
;          ::note-printer)

;(remove-event-handler ::note-printer)

; Other available midi events are:
; * :channel-pressure
; * :control-change
; * :note-off
; * :note-on
; * :pitch-bend
; * :poly-pressure
; * :program-change

; In order to play instruments that continue playing until a key is released,
; we need to keep track of each active synth instance once triggered by :note-on,
; and then send a control message to either kill it or close the gate on an
; envelope so the release starts.  This is what overtone.studio.midi/midi-poly-player
; does for you.  All it requires is that you have exposed an envelope gate as a synth
; parameter called "gate".

(definst poly-ding
  [note 60 amp 1 gate 1]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
    (* amp env snd)))

(definst pad1 [note 60 amp 0.4 amt 0.3 gate 1.0]
  (let [freq (midicps note)
        vel        (+ 0.5 (* 0.5 amp))
        env        (env-gen (adsr 0.01 0.1 0.7 0.5) gate 1 0 1 FREE)
        f-env      (env-gen (perc 1 3))
        src        (pulse [freq (* freq 1.02)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate2       (pulse (* 2 gate (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate2 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
                                        ;reverb     (free-verb compressor 0.5 0.5)
        echo       (comb-n reverb 0.4 0.3 0.5)]
    (* vel env echo)))

; Create a polyphonic midi player
                                        ;(def ding-player (midi-poly-player-core-async pad2))


; and stop it:
                                        ;(midi-player-stop ding-player)
