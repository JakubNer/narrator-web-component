# Narrator W3C Custom Element

## Summary

A narrator Web component, animating text in and out of view to create a cohesive timeline for presenting ideas and
driving other visual component animations.  The animated text is given a period of time to show on screen; hence this
animation is termed "narration".

The component enlarges and highlights text/html as it's flowing in and out of the viewer's attention.  As content flows in and out
of the viewer's attention, events are triggered to let other portions of the Web page know where--in the timeline of
all flows--the page's animations should be.

Each highlighted piece of text is called a "flow".  "Flows" are organized into subsections and sections.  The idea being
that sections are enlarged to bring viewer's attention to all the "flows" it contains.  As a timeline moves forward
"flows" are highlighted, then subsequent sections with their flows, etc..

As each "flow" is highlighted it fires an event with it's "id".

The following capture is the component running from [resources/pubic/index.html](https://github.com/JakubNer/narrator-web-component/blob/master/resources/public/index.html):

![demo](https://github.com/JakubNer/narrator-web-component/blob/master/assets/demo.gif)

The component provides a play/pause button.  All sections are clickable; a click jumps to the
section's start time in the timeline and pauses the narration.  The play/pause button can be
clicked to resume the narration.  As the play/pause state of narration changes, an event with the current flow's "id"
and the current "playing" status is triggered.

If a section contains flows in subsections, the subsections' flows appear in sequence after all of the root sections
flows finish "narrating".

Compnent registered as "narrator".

(The [illustrator component](https://github.com/JakubNer/illustrator-web-component) is related to this one but without the play/pause functionality)

### Attributes

#### sections

JSON:

```
[{"flows":[{"html":"",
            "seconds":0,
            "id":""}],
  "subsections":[{"flows":[]}]}]
```

Each record in list defines a section.  Each section is narrated in order.

A section can have a list of "flows".  Each flow is narrated in order when section is being narrated.  The time
of narrating a section in seconds is defined by "seconds" attribute.  The HTML to render for a flow is defined by "html".
The "id" attribute is the "id" provided to "timeline" event triggered when flow starts narrating.

A section can have subsections to be narrated after all flows for a section--if any--finish narrating.  Subsection flows
are identical to section flows.

While a section is being narrated all other sections are rendered at minimal font-size.

Wile a subsection is being narrated both the section and sub-section are rendered at maximum font-size.

#### paused

Control trick play, "true" if narration should be paused, "false" if should be playing.  Only takes effect when re-triggering narration.

To control trick play while narration occuring, do:

```
kundel.narrator.toggle-play-state(narrator);
```

#### trigger

Change the value of the trigger attribute to restart the narration.

```
kundel.narrator.changed(narrator, "trigger", null, new Date().getTime());
```

Passing a new `Date` object to the attribute `changed` method will restart narration.

#### font-size-min--section

Font size to render sections that are not currently being narrated.

Likely set in terms of `vmin` units for `font-size`.

Subsections that are not currently narrated are not visible.

#### font-size-max--section

Font size to render the currently narrated section and subsection, if any.

#### goto_section

Go to a specific section (by 'id') and pause narration--method only:

```
kundel.narrator.goto_section(narrator,'nam')
```

## Dev Run Application:

```
lein clean
lein figwheel dev
```

Figwheel will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:3449](http://localhost:3449).

## Production Build


To compile clojurescript to javascript:

```
lein clean
lein cljsbuild once min
```

## Connect to REPL (command-line) from IntelliJ

Our 'project.clj' sets up figwheel :nrepl-port to 7002.

Create remote REPL run configuration to 127.0.0.1:7002.

Once started paste the following in the IntelliJ REPL (Alt-8)

```
(use 'figwheel-sidecar.repl-api)
(cljs-repl)
```

## Run Unit Tests

The *dev* build compiles unit tests.

To run them, start the REPL:

```
lein figwheel dev
```

In the REPL source the test runner and run the tests:

```
    (in-ns 'kundel-runner')
    (require '[kundel.runner :as t] :reload)
    (t/run)
```

## Credits

Icons by:

* see /licenses folder (https://www.flaticon.com)
* Daniele De Santis: https://www.flaticon.com/authors/daniele-de-santis
* Pixel Perfect: https://www.flaticon.com/authors/pixel-perfect
* Smashicons: https://www.flaticon.com/authors/smashicons
