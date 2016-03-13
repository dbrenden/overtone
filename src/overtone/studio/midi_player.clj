(ns overtone.studio.midi-player
  (:use [overtone.studio.midi]
        [overtone.sc.node]
        [overtone.sc.dyn-vars])
  (:require [overtone.libs.event :as e]
            [clojure.core.async :as a]
            [clojure.core.reducers :as r]))

(defn midi-poly-player
  "Sets up the event handlers and manages synth instances to easily play
  a polyphonic instrument with a midi controller.  The play-fn should
  take the note and velocity as the only two arguments, and the synth
  should have a gate parameter that can be set to zero when a :note-off
  event is received.

    (definst ding
      [note 60 velocity 100 gate 1]
      (let [freq (midicps note)
            amp  (/ velocity 127.0)
            snd  (sin-osc freq)
            env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
        (* amp env snd)))

    (def dinger (midi-poly-player ding))
  "
  ([play-fn] (midi-poly-player play-fn ::midi-poly-player))
  ([play-fn player-key] (midi-poly-player play-fn [:midi] player-key))
  ([play-fn device-key player-key]
     (let [notes*        (atom {})
           on-event-key  (concat device-key [:note-on])
           off-event-key (concat device-key [:note-off])
           on-key        (concat [::midi-poly-player] on-event-key)
           off-key       (concat [::midi-poly-player] off-event-key)]
       (e/on-event on-event-key (fn [{note :note velocity :velocity}]
                                  (let [amp (float (/ velocity 127))]
                                    (swap! notes* assoc note (play-fn :note note :amp amp :velocity velocity))))
                   on-key)

       (e/on-event off-event-key (fn [{note :note velocity :velocity}]
                                   (let [velocity (float (/ velocity 127 ))]
                                     (when-let [n (get @notes* note)]
                                       (with-inactive-node-modification-error :silent
                                         (node-control n [:gate 0 :after-touch velocity]))
                                       (swap! notes* dissoc note))))
                   off-key)

       ;; TODO listen for '/n_end' event for nodes that free themselves
       ;; before recieving a note-off message.
       (let [player (with-meta {:notes* notes*
                                :on-key on-key
                                :off-key off-key
                                :device-key device-key
                                :player-key player-key
                                :playing? (atom true)}
                      {:type ::midi-poly-player})]
         (swap! poly-players* assoc player-key player)
         player))))


(defn apply-knob-fns
  [state knob-fns params]
  (let [knob-fns-comp (apply comp (map #(partial % state) knob-fns))
        output (knob-fns-comp params)]
    output))


(defn get-change-val
  [state type]
  (dec (* (type state) 2)))

(defn gen-knob-fns
  [midi-conf]
  (map (fn [[type [name base factor]]] (fn [state params]
                                         (let [base-val (if (= :note base)
                                                          (:note params)
                                                          (+ 0.01 base))]
                                           (assoc params name (+ base-val (* factor (get-change-val state type))))))) (seq midi-conf)))

(defonce notes-atom (atom nil))

(defn midi-poly-player-core-async
  "Sets up the event handlers and manages synth instances to easily play
  a polyphonic instrument with a midi controller.  The play-fn should
  take the note and velocity as the only two arguments, and the synth
  should have a gate parameter that can be set to zero when a :note-off
  event is received.

    (definst ding
      [note 60 velocity 100 gate 1]
      (let [freq (midicps note)
            amp  (/ velocity 127.0)
            snd  (sin-osc freq)
            env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
        (* amp env snd)))

    (def dinger (midi-poly-player ding))
  "
  ([play-fn] (midi-poly-player-core-async play-fn nil))
  ([play-fn midi-conf] (midi-poly-player-core-async play-fn ::midi-poly-player midi-conf))
  ([play-fn player-key midi-conf] (midi-poly-player-core-async play-fn [:midi] player-key midi-conf))
  ([play-fn device-key player-key midi-conf]
   (let [mutator-fn-chan (a/chan)
         on-event-key  (concat device-key [:note-on])
         off-event-key (concat device-key [:note-off])
         on-key        (concat [::midi-poly-player] on-event-key)
         off-key       (concat [::midi-poly-player] off-event-key)
         pitch-bend-event-key (concat device-key [:pitch-bend])
         pitch-bend-key (concat [::midi-poly-player] pitch-bend-event-key)
         mod-wheel-event-key (concat device-key [:control-change])
         mod-wheel-key (concat [::midi-poly-player] mod-wheel-event-key)
         midi-conf (or midi-conf {:pitch-bend [:note :note 2] :mod-wheel [:gate 1.5 1.5]})
         knob-fns (gen-knob-fns midi-conf)]
     (a/go-loop [state {:notes {} :pitch-bend 0.5 :mod-wheel 0.5}]
       (let [mutator-fn (a/<! mutator-fn-chan)]
         (recur (mutator-fn state))))
     (e/on-event on-event-key (fn [{note :note velocity :velocity}]
                                (let [amp (float (/ velocity 127))]
                                  (a/go (a/>! mutator-fn-chan
                                              (fn [state]
                                                (assoc-in state [:notes note]
                                                          (apply play-fn (flatten (seq (apply-knob-fns state knob-fns {:note note :amp amp :velocity velocity}))))))))))
                 on-key)

     (e/on-event off-event-key (fn [{note :note velocity :velocity}]
                                 (let [velocity (float (/ velocity 127 ))]
                                   (a/go (a/>! mutator-fn-chan
                                               (fn [state]
                                                 (when-let [n (get-in state [:notes note])]
                                                   (with-inactive-node-modification-error :silent
                                                     (node-control n [:gate 0 :after-touch velocity]))
                                                   (update state :notes dissoc note)))))))
                 off-key)
     (e/on-event pitch-bend-event-key (fn [{shift :data2-f}]
                                        (let [[name base factor] (:pitch-bend midi-conf)]
                                          (a/go (a/>! mutator-fn-chan
                                                      (fn [state]
                                                        (let [change-val (dec (* shift 2))]
                                                          (doseq [[note n] (:notes state)]
                                                            (let [base-val (if (= :note base)
                                                                             note
                                                                             (+ 0.01 base))
                                                                  new-val (+ base-val (* factor change-val))]
                                                              (node-control n [name new-val])))
                                                          (assoc state :pitch-bend shift)))))))
                 pitch-bend-key)

     (e/on-event mod-wheel-event-key (fn [{shift :data2-f}]
                                       (let [[name base factor] (:mod-wheel midi-conf)]
                                         (a/go (a/>! mutator-fn-chan
                                                     (fn [state]
                                                       (let [change-val (dec (* shift 2))]
                                                         (doseq [[note n] (:notes state)]
                                                           (let [base-val (if (= :note base)
                                                                            note
                                                                            (+ 0.01 base))
                                                                 new-val (+ base-val (* factor change-val))] (node-control n [name new-val])))
                                                         (assoc state :mod-wheel shift)))))))
                 mod-wheel-key)
     ;; TODO listen for '/n_end' event for nodes that free themselves
     ;; before recieving a note-off message.
     (let [player (with-meta {:on-key on-key
                              :off-key off-key
                              :device-key device-key
                              :player-key player-key
                              :playing? (atom true)}
                    {:type ::midi-poly-player})]
       (swap! poly-players* assoc player-key player)
       player))))
