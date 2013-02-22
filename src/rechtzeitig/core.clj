(ns rechtzeitig.core
  (:import 
    (java.util Calendar Date GregorianCalendar Locale)
    (java.text ParseException SimpleDateFormat)))


;; 'forward declaration' der verwendeten Funktionen
(declare 
  create-vacation-days
  calculate-easter
  date-map->date-string
  date-string->date-map
  prettify-date-string
  offset-date
  weekend?
  feast?
  workday?
  list-feasts
  valid-date?
  calculate-end
  add-fiction-days
  working-day-col
  no-working-day-col)


;; Festlegung des deutschen Datumsformats "tt.mm.jjjj"
(def SDF (SimpleDateFormat. "dd.MM.yyyy"))


(defn create-holidays
  "Berechnung der beweglichen und unbeweglichen Feiertage eines bestimnten
Jahres"
  [y]
  (let [easter (calculate-easter (int y))]
    [{:type :fix
      :date {:day 1 :month 1 :year y}
      :description "Neujahr (01.01.####)"}
     {:type :fix
      :date {:day 6 :month 1 :year y}
      :description "Heilige Drei Könige (6.1.####)"}
     {:type :moveable-feast
      :date (offset-date  easter -2)
      :description "Karfreitag (beweglich) = Ostern -2"}
     {:type :moveable-feast
      :date (offset-date easter 0)
      :description "Ostersonntag (beweglich) = Ostern +0"}
     {:type :moveable-feast
      :date (offset-date easter +1)
      :description "Ostermontag (beweglich) = Ostern +1"}
     {:type :moveable-feast
      :date (offset-date easter +39)
      :description "Christi Himmelfahrt (beweglich) = Ostern +39"}
     {:type :fixed-feast
      :date {:day 1 :month 5 :year y}
      :description "1.Mai (01.05.####)"}
     {:type :moveable-feast
      :date (offset-date easter +49)
      :description "Pfingstsonntag (beweglich) = Ostern +49"}
     {:type :moveable-feast
      :date (offset-date easter +50)
      :description "Pfingstmontag (beweglich) = Ostern +50"}
     {:type :moveable-feast
      :date (offset-date easter +60)
      :description "Fronleichnam (beweglich) = Ostern +60"}
     {:type :fixed-feast
      :date {:day 3 :month 10 :year y}
      :description "Tag der Deutschen Einheit (03.10.####)"}
     {:type :fixed-feast
      :date {:day 1 :month 11 :year y}
      :description "Allerheiligen (01.11.####)"}
     {:type :fixed-feast
      :date {:day 24 :month 12 :year y}
      :description "Heiligabend (24.12.####)"}
     {:type :fixed-feast
      :date {:day 25 :month 12 :year y}
      :description "1. Weihnachtsfeiertag (25.12.####)"}
     {:type :fixed-feast
      :date {:day 26 :month 12 :year y}
      :description "2. Weihnachtsfeiertag (26.12.####)"}
     {:type :fixed-feast
      :date {:day 31 :month 12 :year y}
      :description "Silvester (31.12.####)"}]))


(defn prettify-date-string
  "Wandelt einen Datumsstring in das Format dd.mm.yyyy um. Aus 1.1.12 wird 01.01.2012"
  [date-string]
  (let [[d m y] (vec (.split date-string  "\\."))]
    (str
     (if (= (count d) 1) (str "0" d) d)
     "."
     (if (= (count m) 1) (str "0" m) m)
     "."
     (if (= (count y) 2) (str "20" y) y))))


(defn date-map->date-string
  "Wandelt ein Datum im Format {:day 2 :month 12 :year 2012} in einen Datumsstring '02.12.2012' um."
  [date-map]
  (prettify-date-string (str (:day date-map) "." (:month date-map) "." (:year date-map))))


(defn date-string->date-map
  "Wandelt einen Datumsstring '02.12.2012' in das Format {:day 2 :month 12 :year 2012} um."
  [date-string]
  (let [[d m y] (map #(Integer/parseInt %) (.split date-string "\\."))]
    {:day d
     :month m
     :year y}))


(defn calculate-easter
  "Berechnung des Osterfeiertags eines bestimmten Jahres"
  [year]
  (let [d (+ (mod (- (- 255 (* 11 (mod year 19))) 21) 30) 21)
        x (if (> d 48) 1 0)
        t (- (+ d x 6) (mod (+ year (int (/ year 4)) d x 1) 7))
        gc (Calendar/getInstance Locale/GERMAN)]
    (doto gc
      (.set year 2 1)
      (.add Calendar/DATE t))
    {:day   (. gc get Calendar/DAY_OF_MONTH)
     :month (+ (. gc get Calendar/MONTH) 1)
     :year  (. gc get Calendar/YEAR)}))


(defn offset-date
  "Berechnung eines Datums x Tage vor oder nach dem beweglichen Festtag Ostern"
  [date-map count-incdrec]
  (let [gc (Calendar/getInstance Locale/GERMAN)]
    (doto gc
      (.set (date-map :year) (- (date-map :month) 1) (date-map :day))
      (.add Calendar/DATE count-incdrec))
    {:day   (. gc get Calendar/DAY_OF_MONTH)
     :month (+ (. gc get Calendar/MONTH) 1)
     :year  (. gc get Calendar/YEAR)}))


(defn weekend?
  "Handelt es sich bei dem Datum um einen Wochenendtag (Samstag oder Sonntag)"
  [date-map]
  (let [gc (GregorianCalendar. (date-map :year) (- (date-map :month) 1) (date-map :day))]
    (or (= (. gc get Calendar/DAY_OF_WEEK) 1)
        (= (. gc get Calendar/DAY_OF_WEEK) 7))))


(defn feast?
  "Handelt es sich bei dem Datum um einen beweglichen oder
unbeweglichen Festtag?
Dabei wird überprüft ob das Datum in der übergebenen Map der Feiertage
`vc` enthalten ist"
  [date-map vc]
  (some #(= % date-map) (map :date vc)))


(defn workday?
  "Handelt es sich bei dem Datum `[y m d vc]` um einen Arbeitstag?
Dabei wird überprüft ob das Datum kein Wochendtag ist und nicht in
der übergebenen Map der Feiertage `vc` enthalten ist"
  [date-map vc]
  (not (or (weekend? date-map) (feast? date-map vc))))


(defn list-feasts
  "Liefert eine lesbare Auflistung der beweglichen und unbeweglichen Feiertage"
  [vc]
  (doseq [x vc]
    (println (:description x) " : " (:date x))))


(defn valid-date?
  "Prüft, ob es es sich bei dem Argument (`String`) um ein gültiges Datum handelt."
  [^String date]
  (if (= date (re-matches #"\d{1,2}\.\d{1,2}\.\d{4}" date))
    (let [date-map1                (date-string->date-map date)
          {:keys [day month year]} date-map1
          gc                       (GregorianCalendar. year (- month 1) day)
          date-map2                {:day   (. gc get Calendar/DAY_OF_MONTH)
                                    :month (+ (. gc get Calendar/MONTH) 1)
                                    :year  (. gc get Calendar/YEAR)}]
      (= date-map1 date-map2))))


(defn calculate-end
  "Berechnet das Fristende. Wird nur ein Datum übergeben, dann wird der
Fristbeginn über die 3-Tages-Fiktion der Zustellung ermittelt."
  ([date1]
     (when (valid-date? date1)
       (calculate-end date1
                      (date-map->date-string (offset-date (date-string->date-map date1)
                                                          3)))))
  ([date1 date2]
     (when (and (valid-date? date1) (valid-date? date2))
       (let [{:keys [day month year]} (date-string->date-map date2)
             gc                       (GregorianCalendar. year (- month 1) day)  
             vc                       (create-holidays year)]
         (do 
           (.add gc Calendar/MONTH 1)
           (while (not (workday? {:year (.get gc Calendar/YEAR)
                                  :month (+ (.get gc Calendar/MONTH) 1)
                                  :day (.get gc Calendar/DAY_OF_MONTH)}
                                 vc))
             (.add gc Calendar/DATE 1)))
         {:letter-date (date-string->date-map date1)
          :begin-date  {:day day
                        :month month
                        :year year}
          :end-date    {:day (.get gc Calendar/DAY_OF_MONTH)
                        :month (+ (.get gc Calendar/MONTH) 1)
                        :year (.get gc Calendar/YEAR)}
          }))))


(defn working-day-col
  "Liefert einen `vector` aller Arbeitstage eines Jahres."
  [year]
  (let [vc (create-holidays year)
        gc (GregorianCalendar. year 0 1)]
    (loop [col []]
      (let [date-map {:day   (.get  gc Calendar/DAY_OF_MONTH)
                      :month (+ (.get gc Calendar/MONTH) 1)
                      :year  (.get gc Calendar/YEAR) }]
        (if (= year (:year date-map))
          (do
            (.add gc Calendar/DATE 1)
            (if (workday? date-map vc)
              (recur (conj col date-map))
              (recur col)))
          col)))))


(defn no-working-day-col
  "Liefert einen `vector` aller Nicht-Arbeitstage eines Jahres."
  [year]
  (let [vc (create-holidays year)
        gc (GregorianCalendar. (int year) 0 1)]
    (loop [col []]
      (let [date-map {:day   (. gc get Calendar/DAY_OF_MONTH)
                      :month (+ (.get gc Calendar/MONTH) 1)
                      :year  (.get gc Calendar/YEAR) }]
        (if (= year (:year date-map))
          (do
            (.add gc Calendar/DATE 1)
            (if (not (workday? date-map vc))
              (recur (conj col date-map))
              (recur col)))
          col)))))



  

