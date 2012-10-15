

(setq player "xmms")

(setq play        "-p")
(setq stop        "-s")
(setq fwd         "-f")
(setq rew         "-r")
(setq play-pause  "-t")


(defun player-exec (task)
  (system (concat player " " task)))

(defun player-play-pause ()
  (interactive)
  (player-exec play-pause))

(defun player-stop ()
  (interactive)
  (player-exec stop))

(defun player-fwd ()
  (interactive)
  (player-exec fwd))

(defun player-rew ()
  (interactive)
  (player-exec rew))

(bind-keys global-keymap
	  "XF86AudioPlay" 'player-play-pause
	  "XF86AudioStop" 'player-stop
	  "XF86AudioNext" 'player-fwd
	  "XF86AudioPrev" 'player-rew
	  )



