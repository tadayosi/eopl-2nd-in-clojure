(ns eopl.chap2.sec24-test
  (:use clojure.test
        eopl.chap2.sec24))

;; queue
(deftest test-queue
  (is (thrown? Exception ((queue-get-dequeue-operation (create-queue)))))
  (is (= 34 (let [q1 (create-queue) q2 (create-queue)]
              (let [enq1 (queue-get-enqueu-operation q1)
                    enq2 (queue-get-enqueu-operation q2)
                    deq1 (queue-get-dequeue-operation q1)
                    deq2 (queue-get-dequeue-operation q2)]
                (do
                  (enq1 33)
                  (enq2 (+ 1 (deq1)))
                  (deq2)))))))
