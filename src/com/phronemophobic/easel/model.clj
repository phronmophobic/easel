(ns com.phronemophobic.easel.model)


(defprotocol IResizable
  (-resize [this [w h] content-scale]))

(defprotocol IUI
  (-ui [this {:keys [$context context
                     $shared shared]}]))

(defprotocol IApplet
  (-start [this {:keys [$ref
                        size content-scale
                        context $context
                        shared $shared]}])
  (-stop [this]))

(defprotocol IEasel
  (-add-applet [this info])
  (-remove-applet [this id])
  (-visible-applets [this])
  #_(-ui [this])
  (-show-applet [this id])
  (-hide-applet [this id])
  (-applets [this]))

