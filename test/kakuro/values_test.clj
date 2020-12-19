(ns kakuro.values-test
  (:require
   [kakuro.values :as vas]
   [clojure.test :refer :all]
   ))

(deftest values-unique?-test
  (testing "positive cases"
    (is (vas/values-unique? '(#{1} #{2} #{3} #{4}#{5}#{6}#{7}#{8}#{9})))
    (is (vas/values-unique? '(#{1} #{2} #{3} #{4})))
    )
  (testing "negative"
    (is (not (vas/values-unique? '(#{1} #{1}))))
    (is (not (vas/values-unique? '(#{1} #{2} #{3} #{4}#{5}#{6}#{7}#{8}#{9} #{2}))))
    (is (not (vas/values-unique? '(#{1 2 3} #{2} #{3} #{4}#{5}#{6}#{7}#{8}#{9}))))
    ))
    

(deftest get-value-sets-test
  (testing "unique values"
    (is (= (vas/get-value-sets
            {{:x 1 :y 2} #{2}
             {:x 2 :y 2} #{3}
             {:x 3 :y 2} #{1}
             {:x 1 :y 3} #{1}
             {:x 2 :y 3} #{2}
             {:x 3 :y 3} #{6}
             {:x 1 :y 4} #{6}
             {:x 3 :y 4} #{7}  }
            [{:x 1 :y 3} {:x 1 :y 4}])
           [#{1} #{6}]))))

