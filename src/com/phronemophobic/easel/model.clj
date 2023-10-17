(ns com.phronemophobic.easel.model)


(defprotocol IResizable
  (-resize [this [w h] content-scale]))

(defprotocol IApplet
  (-start [this $ref])
  (-stop [this])
  (-ui [this $context context]))

(defprotocol IEasel
  (-add-applet [this applet])
  (-remove-applet [this id])
  (-layout-direction [this] [this dir])
  (-visible-applets [this])
  (-show-applet [this id])
  (-hide-applet [this id])
  (-applets [this]))

