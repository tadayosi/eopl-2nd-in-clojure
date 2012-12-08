(ns eopl.chap2.sec24)

;; queue
(defn create-queue []
  (let [q-in (atom '())
        q-out (atom '())]
    (let [reset-queue (fn []
                        (reset! q-in '())
                        (reset! q-out '()))
          empty-queue? (fn []
                         (and (empty? @q-in)
                              (empty? @q-out)))
          enqueue (fn [x]
                    (reset! q-in (cons x @q-in)))
          dequeue (fn []
                    (if (empty-queue?)
                      (throw (Exception. (str 'dequeue ": Not on an empty queue")))
                      (do
                        (if (empty? @q-out)
                          (do
                            (reset! q-out (reverse @q-in))
                            (reset! q-in '())))
                        (let [ans (first @q-out)]
                          (reset! q-out (rest @q-out))
                          ans))))]
      [reset-queue empty-queue? enqueue dequeue])))
(defn queue-get-reset-operation [q]
  (q 0))
(defn queue-get-empty?-operation [q]
  (q 1))
(defn queue-get-enqueu-operation [q]
  (q 2))
(defn queue-get-dequeue-operation [q]
  (q 3))
