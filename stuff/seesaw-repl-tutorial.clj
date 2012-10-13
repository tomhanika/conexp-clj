; A REPL-based, annotated Seesaw tutorial
; Please visit https://github.com/daveray/seesaw for more info
;
; This is a very basic intro to Seesaw, a Clojure UI toolkit. It covers
; Seesaw's basic features and philosophy, but only scratches the surface
; of what's available. It only assumes knowledge of Clojure. No Swing or
; Java experience is needed.
;
; This material was first presented in a talk at @CraftsmanGuild in
; Ann Arbor, MI.
;
; Dave Ray, December 2011


; First install Leiningen (https://github.com/technomancy/leiningen) and 
; create a new project. In a terminal:
;
;    $ lein new hello-seesaw
;    $ cd hello-seesaw
;    $ edit project.clj and add [seesaw "1.2.2"] to :dependencies
;    $ lein deps

; Now let's start up the REPL and do some Seesaw stuff. Repl results are
; marked with ;=> in the usual way.
$ lein repl

; Here we go. First will use Seesaw and stuff
(use 'clojure.repl)
;=> nil
(use 'seesaw.core)
;=> nil

; Now before we create any UI stuff, tell Seesaw to try to make things look as
; native as possible. This puts the menubar in the right place on Mac, etc.
(native!)
;=> nil

; Now we'll start by making a frame to put stuff in. Most Seesaw widgets are
; constructed with simple functions that take :keyword/value pairs.
; (seesaw.core/frame) creates a new frame.
(def f (frame :title "Get to know Seesaw"))
;=> #'user/f

; So now we have a frame, but we haven't displayed it yet. Usually, we
; want to pack and show a frame. pack! just auto-sizes the frame for its
; contents
(-> f pack! show!)
;=> #<JFrame ... >

; Note that pack! and show! both return their argument so they can be chained.
; This is true of most Seesaw functions with side-effects.
;
; At this point you should have a very small, very boring window on your screen.
;
;                   
;               +-----------------------------+
;               | Get to know Seesaw         x|
;               +-----------------------------+
;               |                             |
;               +-----------------------------+
;

; The properties of a widget can be queried ...
(config f :title)
;=> "Get to know Seesaw"

; ... and modified
(config! f :title "No RLY, get to know Seesaw!")
;=> #<JFrame ...>

; Note that the title of the frame changed!
;                   
;               +------------------------------+
;               | NO RLY, get to know Seesaw  x|
;               +------------------------------+
;               |                              |
;               +------------------------------+


; So we have an empty frame. Let's give it some content
(config! f :content "This is some content")
;=> #<JFrame ... >
;                   
;               +------------------------------+
;               | NO RLY, get to know Seesaw  x|
;               +------------------------------+
;               | This is some content         |
;               +------------------------------+

; Now we have a frame with a label in it. When Seesaw sees something like a 
; string, it will create a label, mostly out of habit. Of course, we could
; create a label ourselves ...
(def lbl (label "I'm another label"))
;=> #'user/lbl

; and show it in the frame
(config! f :content lbl)
;=> #<JFrame ... >

; You know, we're going to be doing that a lot. Let's make a function
(defn display [content]
  (config! f :content content)
  content)
;=> #'user/display

(display lbl)

; Like the frame, a label can be manipulated with (config!). We can set some
; colors
(config! lbl :background :pink :foreground "#00f")
;=> #<JLabel ... >

; Seesaw knows about CSS color names and codes. Notice how we can set as many
; properties as we want with one (config!) call

; We can change the font too
(config! lbl :font "ARIAL-BOLD-21")
;=> #<JLabel ... >

; "FAMILY-STYLE-SIZE" is conventient, but using (seesaw.font/font) we get a 
; little more control
(use 'seesaw.font)
;=> nil
(config! lbl :font (font :name :monospaced 
                         :style #{:bold :italic} 
                         :size 18))
;=> #<JLabel ... >


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; So we know a little about labels now. How about a button?
(def b (button :text "Click Me"))
;=> #'user/b

; Sometimes we might want to show a message ...
(alert "I'm an alert")
;=> nil

; or get input from the user ...
(input "What's your favorite color?")
;=> "Blue"

; Notice that both functions block until the popup is dismissed. They both
; take an additional, initial argument which indicates which frame they're
; associated with. Otherwise, they'll just pop up in the middle of your screen


; Let's get back to our button and get him doing something...
(display b)
;=> #<JButton>
;
;               +------------------------------+
;               | NO RLY, get to know Seesaw  x|
;               +------------------------------+
;               |+----------------------------+|
;               ||                            ||
;               ||          Click Me          ||
;               ||                            ||
;               |+----------------------------+|
;               +------------------------------+
;
; Click it. Nothing happens.

; Seesaw's (listen) function let's us register handler functions for events on
; a widget. All buttons support an :action event. So,
(listen b :action (fn [e] (alert e "Thanks!")))
;=> #<core$juxt$fn__3775 clojure.core$juxt$fn__3775@2e46638f> 

; Now we can click the button and see something happen. The 'e' parameter
; is an event object. Most of the time in Seesaw, an event object can be used
; as a proxy for the widget that triggered the event. In this case, the button
; becomes the "parent" of the alert, so it's placed nicely over our frame.

; Also note that (listen) returned a function. Calling this function will
; undo the effects of the (listen) call, i.e. unregister the listener
(*1)
;=> [...]

; Now clicking does nothing again.

; (listen) can register multiple event handlers at once
(listen b :mouse-entered #(config! % :foreground :blue)
          :mouse-exited  #(config! % :foreground :red))
;=> #<core$juxt$fn__3775 clojure.core$juxt$fn__3775@2e46638f> 

; Move the mouse over the buttons to see the text color change. Again note
; that we're using the event parameter as a proxy for the button. This
; time with (config!).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's talk about a slightly more interesting widget, a listbox.
(def lb (listbox :model (-> 'seesaw.core ns-publics keys sort)))
;=> #<JList ...>
(display lb)
;=> #<JList ...>
;
;               +------------------------------+
;               | NO RLY, get to know Seesaw  x|
;               +------------------------------+
;               |#abtract-panel################|
;               | action                       |
;               | add!                         |
;               | add!*                        |
;               | alert                        |
;               +------------------------------+

; (listbox)'s most important option is :model which can take any (finite)
; sequence and display it. Items are displayed using (str) unless a custom
; :renderer is used.

; This listbox is ok, but our list doesn't fit in the frame and there's no
; scrollbars. You can add scrolling to most widgets with (scrollable) ...
(display (scrollable lb))
;=> #<JScrollPane ... >

; Now that we have a list, it'd be nice to know what its selection is. Seesaw
; has a unified nice selection interface. If there's no selection we get nil
(selection lb)
;=> nil

; Otherwise, we get the value:
(selection lb)
;=> abstract-panel
(type *1)
;=> clojure.lang.Symbol

; Note we get the same objects out that we put in the :model above.

; If we want multi-selection (shift/ctrl-click)
(selection lb {:multi? true})
;=> (action add! add!* alert)

; Similarly, we can set the selection
(selection! lb 'all-frames)
;=> #<JList ... >

; was all-frames selected for you too?

; Finally, we might want to know when the selection changes. Everybody
; supports the :selection event
(listen lb :selection (fn [e] (println "Selection is " (selection e))))
;=> #<core$juxt$fn__3775 clojure.core$juxt$fn__3775@2e46638f> 

; click around on the listbox and watch the selection print out.

; and unregister as usual
(*1)
;=> [...]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now we come to editable text widgets. It's easy to make one
(def field (display (text "This is a text field.")))
;=> #<JTextField ... >
;
;               +------------------------------+
;               | NO RLY, get to know Seesaw  x|
;               +------------------------------+
;               |                              |
;               |                              |
;               | This is a text field|        |
;               |                              |
;               |                              |
;               +------------------------------+

; A text field is a single line of text. You can query the text...
(text field)
;=> "This is a text field."

; ... and change it programmatically
(text! field "A new value")
;=> #<JTextField ... >

; ... and you can set the font, etc as usual with config
(config! field :font "MONOSPACED-PLAIN-12" :background "#f88")
;=> #<JTextField ... >


; If you've got more than one line, you can make it multi-line...
(def area (text :multi-line? true :font "MONOSPACED-PLAIN-14"
                :text "This
is 
multi
line
text"))
;=> #<JTextArea ... >

(display area)
;=> #<JTextArea ... >
;
;               +------------------------------+
;               | NO RLY, get to know Seesaw  x|
;               +------------------------------+
;               | This                         |
;               | is                           |
;               | multi                        |
;               | line|                        |
;               +------------------------------+


; If you've got a reader, URL, or anything else slurp-able, you can fill up the
; text area with it
(text! area (java.net.URL. "http://clojure.com"))
;=> #<JTextArea ... >

; Of course, we need scrollbars now
(display (scrollable area))
;=> #<JTextArea ... >

; Like selection, Seesaw has a unified scrolling API. You can scroll to the top ...
(scroll! area :to :top)
;=> #<JTextArea ... >

; ... or the bottom ...
(scroll! area :to :bottom)
;=> #<JTextArea ... >

; ... or to a particular line ...
(scroll! area :to [:line 50])
;=> #<JTextArea ... >


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's try showing two widgets at once. One option is a splitter:
(def split (left-right-split (scrollable lb) (scrollable area) :divider-location 1/3))
;=> #'user/split

(display split)
;=> #<JSplitPane ... >
;
;
;             +-------------------------------------+
;             | NO RLY, get to know Seesaw         x|
;             +-----------+-------------------------+
;             |abstract-pa| This                    |
;             |action     | is                      |
;             |add!#######| multi                   |
;             |add!*      | line|                   |
;             +-----------+-------------------------+
;

; This shows both our listbox on the left and text area on the right with a 
; movable splitter. If you're familiar with Swing, you'll recognize what an
; achievement :divider-location is.

; Let's hook them together. First a function to grab a doc string
(defn doc-str [s] (-> (symbol "seesaw.core" (name s)) resolve meta :doc))
;=> #'user/doc-str

; ... and now the usual selection handler
(listen lb :selection
        (fn [e]
          (when-let [s (selection e)]
            (-> area
              (text!   (doc-str s))
              (scroll! :to :top)))))
;=> #<JList ... >

; note how we have to be sure to check for nil selection


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ok. We're pushing on the limits of what we can comfortably type in a REPL,
; so just a couple more things ...

; Imagine we wanted to show source as well as docs. A set of radio button might be
; a nice way to switch betwen them ...

(def rbs (for [i [:source :doc]] 
           (radio :id i :class :type :text (name i))))
;=> #'user/rbs

; we have a couple radio buttons. Let's add them to our frame. We'll use a 
; border-panel to lay things out. This is what a border-panel layout looks like:
;
;     +------------------------------------------+
;     |                 :north                   |
;     |                                          |
;     +----------+-------------------+-----------+
;     |          |                   |           |
;     |          |                   |           |
;     | :west    |       :center     |   :east   |
;     |          |                   |           |
;     |          |                   |           |
;     +----------+-------------------+-----------+
;     |                                          |
;     |                 :south                   |
;     +------------------------------------------+
;


(display (border-panel
           :north (horizontal-panel :items rbs)
           :center split
           :vgap 5 :hgap 5 :border 5))
;=> #<JPanel ...>

; The :vgap, :hgap, and :border options just make things look a little nicer.

; If you click the radio buttons, you'll notice a little problem. They're not
; mutually exclusive. We'll put them in a button group to fix that, but first
; a small detour...

; How can we get to the radio buttons without using the rbs var? How about
; with a selector:
(select f [:JRadioButton])
;=> (#<JRadioButton ... > #<JRadioButton ... >)

; or, since we gave them a :class option above ...
(select f [:.type])
;=> (#<JRadioButton ... > #<JRadioButton ... >)

; or, we can get them individually by :id
(select f [:#source])
;=> #<JRadioButton ... >

; Selectors are always enclosed in a vector.

; Now, about those buttons. We'll need a button group...
(def group (button-group))
;=> #'user/group

; It happens that (config!) can take a sequence as its first argument in addition
; to a single widget ...
(config! (select f [:.type]) :group group)
;=> (#<JRadioButton ... > #<JRadioButton ... >)

; Now our buttons are nice and mutex-y. A button group, like most things, has a
; selection too, whichever button is currently selected:
(selection group)
;=> #<JRadioButton ...>

; Combine this with (id-of) and we have our original keywords back:
(-> group selection id-of)
;=> :source
(-> group selection id-of)
;=> :doc

; and, of course, you can register a listener for group selection changes
(listen group :selection 
        (fn [e] 
          (when-let [s (selection group)] 
            (println "Selection is " (id-of s)))))

; Now open hello-seesaw.core.clj and see if you can put this all together into
; an app.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For more info see the Seesaw wiki:
;    https://github.com/daveray/seesaw/wiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

