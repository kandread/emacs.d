;; -*- lexical-binding: t; -*-

;; Bindings to ZMQ sockets
(use-package zmq)

;; Interface to communicate with Jupyter kernels
(use-package emacs-jupyter
  :quelpa (emacs-jupyter :fetcher github :repo "dzop/emacs-jupyter"))
