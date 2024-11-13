(import hyrule *)
(import re)
(require hyrule * :readers *)

(setv line-matcher (re.compile #[[# \d+ "(.*)"( (\d))?( (\d))?]]))

(setv sys-status False)
(defn check-line [line]
  (global sys-status)
  (when (and (line.startswith "typedef")
             (line.endswith "Py_ssize_t;"))
    (return False))
  (let [matches (line-matcher.fullmatch line)]
    (when matches
      (let [options (list (->> [(get matches 2)
                                (get matches 4)]
                                (filter #%(not (is %1 None)))
                                (map int)))]
        (setv sys-status (in 3 options)))
      (return False))
    (when (line.startswith "#")
      (return False))
    (not sys-status)))

(let [f (open "PythonPre.h")]
  (for [raw-line (f.readlines)
        :setv line (.rstrip raw-line)]
    (when (and line (check-line line))
      (print line))))
