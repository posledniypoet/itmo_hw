; :NOTE: # Не поддержки вывода даты/времени
(definterface ILogger
              (log [message]))

(deftype ConsoleLogger
         []
         ILogger
         (log [this message] (println message)))

(deftype HTMLLogger
         [filename]
         ILogger
         (log [this message] (spit filename (str "<p>" message "</p>\n") :append true)))

(deftype CompositeLogger
         [loggers]
         ILogger
         (log [this message] (doall (map #(.log % message) loggers))))

(def logLevels [:Debug :Info :Warning :Error])

(def logLevelToString
  (apply hash-map (flatten (map #(vector %1 (name %1)) logLevels))))

(def logLevelToInt
  (apply hash-map (flatten (map-indexed #(vector %2 %1) logLevels))))

(definterface ILevelLogger
              (log [level message]))

(deftype LevelLogger
         [mode-level logger]
         ILevelLogger
         (log [this level message] (if
                                     (>= (level logLevelToInt) (mode-level logLevelToInt))
                                     (.log logger (str (level logLevelToString) ": " message))
                                     )
              ))

(.log (LevelLogger. :Warning (CompositeLogger. [(HTMLLogger. "log.html") (ConsoleLogger.)])) :Error "error")
