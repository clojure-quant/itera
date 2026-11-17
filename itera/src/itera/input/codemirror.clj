(ns itera.input.codemirror
  "CodeMirror 6 editor (Clojure mode via @nextjournal/clojure-mode) as a Hyper
   client component, plus `codemirror-ui`, a server helper binding it to a
   caller cursor.

   The editor is browser-side: it reads its document from the observed raw
   `code` attribute and, on blur, emits a `code-change` CustomEvent whose
   `detail` is the current document.  The caller turns that into an action that
   writes the cursor, re-rendering the page.

   The CodeMirror dep URLs are pinned and deps-deduped so the editor shares a
   single @codemirror/state, @codemirror/view and @codemirror/language instance
   with @nextjournal/clojure-mode (esm.sh encodes the dep pins into the module
   path; mismatched pins yield duplicate instances and rejected extensions).

   Must be called inside a Hyper render context."
  (:require [hyper.core :as h]))

(h/defc itera-codemirror
  "CodeMirror 6 editor (Clojure mode) web component.  Observed attr: code."
  {:require [["https://esm.sh/@nextjournal/clojure-mode@0.3.5?deps=@codemirror/state@6.5.2,@codemirror/view@6.36.4,@codemirror/language@6.11.0,@codemirror/commands@6.8.1" :as clj-mode]
             ["https://esm.sh/@codemirror/view@6.36.4?deps=@codemirror/state@6.5.2" :as cm-view]
             ["https://esm.sh/@codemirror/state@6.5.2" :as cm-state]
             ["https://esm.sh/@codemirror/language@6.11.0?deps=@codemirror/state@6.5.2,@codemirror/view@6.36.4" :as cm-lang]]}
  [{:keys [code]}]
  (render
   [:div {:class "itera-codemirror"
          :style "height:100%;width:100%;overflow:auto"}])
  (mount [root]
    (let [EditorState (.-EditorState cm-state)
          EditorView  (.-EditorView cm-view)
          keymap      (.-keymap cm-view)
          host-el     (.-host (.getRootNode root))
          raw-code    (.getAttribute host-el "code")
          host        (.querySelector root ".itera-codemirror")
          base        (array
                       (.of keymap clj-mode/complete_keymap)
                       (cm-view/lineNumbers)
                       (cm-view/highlightActiveLine)
                       (cm-view/drawSelection)
                       (.-lineWrapping EditorView)
                       (cm-lang/syntaxHighlighting cm-lang/defaultHighlightStyle)
                       (.theme EditorView
                               (js-obj "&" (js-obj "height" "100%")
                                       ".cm-scroller" (js-obj "overflow" "auto")
                                       ".cm-matchingBracket" (js-obj "color" "#0b0")
                                       ".cm-nonmatchingBracket" (js-obj "color" "#a22"))))
          extensions  (.concat base clj-mode/default_extensions)
          state       (.create EditorState
                               (js-obj "doc" (or raw-code "")
                                       "extensions" extensions))
          ;; clojure-mode 0.3.5's bracket-matching theme uses `$`-prefixed
          ;; selectors the current CodeMirror style engine cannot parse.  In a
          ;; shadow root CodeMirror mounts styles via adoptedStyleSheets +
          ;; insertRule, so that one bad rule throws and aborts EditorView
          ;; construction; on a plain document the same rule is silently
          ;; ignored (a <style> tag).  Ignore such rules while mounting, and
          ;; insert an inert rule in their place so the style engine's rule
          ;; indices (which it advances even when insertRule throws) stay in
          ;; sync and later rules are not dropped.
          proto       (.-prototype js/CSSStyleSheet)
          insert-rule (when proto (.-insertRule proto))
          guard       (fn [rule idx]
                        (this-as sheet
                          (try
                            (.call insert-rule sheet rule idx)
                            (catch :default _
                              (.call insert-rule sheet ":root{}" idx)))))
          view        (do
                        (when insert-rule
                          (set! (.-insertRule proto) guard))
                        (try
                          (new EditorView
                               (js-obj "state" state "parent" host
                                       "extensions" extensions))
                          (finally
                            (when insert-rule
                              (set! (.-insertRule proto) insert-rule)))))
          on-blur     (fn [_]
                        (let [doc (.toString (.-doc (.-state view)))]
                          (when (not= doc (.-synced ctx))
                            (set! (.-synced ctx) doc)
                            (emit "code-change" doc))))]
      (set! (.-hostEl ctx) host-el)
      (set! (.-view ctx) view)
      (set! (.-synced ctx) (or raw-code ""))
      (set! (.-onBlur ctx) on-blur)
      (.addEventListener (.-contentDOM view) "blur" on-blur)))
  (update [_root]
    (let [view    (.-view ctx)
          host-el (.-hostEl ctx)]
      (when (and view host-el)
        (let [incoming (or (.getAttribute host-el "code") "")]
          (when (not= incoming (.-synced ctx))
            (set! (.-synced ctx) incoming)
            (.dispatch view
                       (js-obj "changes"
                               (js-obj "from" 0
                                       "to" (.-length (.-doc (.-state view)))
                                       "insert" incoming))))))))
  (unmount [_root]
    (when-let [view (.-view ctx)]
      (when-let [f (.-onBlur ctx)]
        (.removeEventListener (.-contentDOM view) "blur" f))
      (.destroy view))
    (set! (.-view ctx) nil)
    (set! (.-onBlur ctx) nil)))

(defn codemirror-ui
  "Render a CodeMirror (Clojure mode) editor bound to code-a (a hyper
   cursor/atom whose value is the code string).  On blur the editor emits the
   current document; the action writes code-a, re-rendering the page.

   opts: {:style <host style map> :class <host class>}.  Must be called inside
   a Hyper render context."
  ([code-a] (codemirror-ui code-a nil))
  ([code-a opts]
   (let [{:keys [style class]} opts]
     (itera-codemirror
      (cond-> {:code (str (or @code-a ""))
               :data-on:code-change
               (h/action (reset! code-a $detail))}
        true  (assoc :style (merge {:width "100%" :height "320px"} style))
        class (assoc :class class))))))
