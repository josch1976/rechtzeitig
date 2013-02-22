(ns rechtzeitig.core-test
  (:use clojure.test
        rechtzeitig.core))

(deftest test-create-holidays
   (testing "Erzeugt die Liste der Feiertage"
     (is (=  (:date (nth (create-holidays 2012) 3))
             {:day 8 :month 4 :year 2012}))
     (is (=  (count (create-holidays 2012))
             16))))

(deftest test-calculate-easter
  (testing "Berechnet den Osterfeiertag"
    (is (= (calculate-easter 2012)
           {:day 8 :month 4 :year 2012}))
    (is (= (calculate-easter 2010)
           {:day 4 :month 4 :year 2010}))))

(deftest test-before-after-easter
  (testing "addiert oder subtrahiert x Tage zum Ostersonntag"
    (is (= (before-after-easter {:day 8 :month 4 :year 2012} 60)
           {:day 7, :month 6, :year 2012})))
    (is (= (before-after-easter {:day 8 :month 4 :year 2012} -60)
           {:day 8, :month 2, :year 2012})))

(deftest test-weekend?
  (testing "Handelt es sich um einen Wochendtag?"
    (is (= (weekend? 2012 1 25)
           false))
    (is (= (weekend? 2012 1 22)
           true))))

(deftest test-feast?
  (testing "Prüft, ob es sich um einen Feiertag handelt"
    (is (= (feast? 2012 1 25 (create-holidays 2012))
           nil))
    (is (= (feast? 2012 4 8 (create-holidays 2012))
           true))))

(deftest test-workday?
  (testing "Prüft, ob es sich um einen Arbeitstag handelt"
    (is (= (workday? 2012 1 25 (create-holidays 2012))
           true))
    (is (= (workday? 2012 4 8 (create-holidays 2012))
           false))
    (is (= (workday? 2012 1 22 (create-holidays 2012))
           false))))

(deftest test-valid-date?
  (testing "Überprüfung des Datumstrings"
    (is (= (valid-date? "1.1.2012")
           true))
    (is (= (valid-date? "1.10.2012")
           true))
    (is (= (valid-date? "10.10.2012")
           true))
    (is (= (valid-date? "01.1.2012")
           true))
    (is (= (valid-date? "29.2.2012")
           true))
    (is (= (valid-date? "29.2.2011")
           false))
    (is (= (valid-date? "32.1.2012")
           false))
 ;; (is (= (valid-date? "100.1.2012")
 ;;        false))
    (is (= (valid-date? "10.13.2012")
           false))))
(deftest test-extract-to-y-m-d
  (testing "Umwandlung des Datums in einen Vector"
      (is (= (extract-to-y-m-d "01.01.2012")
             [2012 1 1]))
      (is (= (extract-to-y-m-d "1.01.2012")
             [2012 1 1]))
      (is (= (extract-to-y-m-d "20.01.2012")
             [2012 1 20]))
      (is (= (extract-to-y-m-d "20.1.2012")
             [2012 1 20]))))

(deftest test-calculate-end
  (testing "Berechnung des Fristendes"
    (is (= (calculate-end "26.2.2012")
           [2012 3 29]))
    (is (= (calculate-end "26.2.2012" "28.02.2012")
           [2012 3 28]))
    (is (= (calculate-end "29.1.2012")
           [2012 3 1]))
    (is (= (calculate-end "27.1.2012")
           [2012 2 29]))))

(deftest test-format-dd-mm-yyyy
  (testing "Umwandlung in `String`-Format"
    (is (= (format-dd-mm-yyyy [2012 1 1])
           "01.01.2012"))
    (is (= (format-dd-mm-yyyy [12 1 1])
           "01.01.2012"))
    (is (= (format-dd-mm-yyyy [2012 1 10])
           "10.01.2012"))))