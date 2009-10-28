(use 'conexp)
(import 'javax.swing.JFrame)

(let [frame (conexp-gui)]
  (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))
