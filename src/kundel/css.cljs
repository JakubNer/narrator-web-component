(ns kundel.css)

(defn get-styles [font-min font-max]
  (str "

  .narrator-sections {
    height: 100%;
    width: 100%;
    display: flex;
    flex-direction: column;
    justify-content: space-evenly;
  }

  .narrator-sections-center {
    position: fixed;
    top: 50%;
    left: 50%;
    width: 0px;
    height: 0px;
  }

  .narrator-sections-center-play {
    position: absolute;
    top: -128px;
    left: -128px;
    width: 256px;
    height: 256px;
    -webkit-transition: opacity .5s linear;
    -moz-transition: opacity .5s linear;
    -o-transition: opacity .5s linear;
    transition: opacity .5s linear;
    background-size: 256px 256px;
    background: url('data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTkuMC4wLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDQyMCA0MjAiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDQyMCA0MjA7IiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iMjU2cHgiIGhlaWdodD0iMjU2cHgiPgo8Zz4KCTxwYXRoIGQ9Ik0yMTAsMjFjMTA0LjIxNiwwLDE4OSw4NC43ODQsMTg5LDE4OXMtODQuNzg0LDE4OS0xODksMTg5UzIxLDMxNC4yMTYsMjEsMjEwUzEwNS43ODQsMjEsMjEwLDIxIE0yMTAsMCAgIEM5NC4wMzEsMCwwLDk0LjAyNCwwLDIxMHM5NC4wMzEsMjEwLDIxMCwyMTBzMjEwLTk0LjAyNCwyMTAtMjEwUzMyNS45NjksMCwyMTAsMEwyMTAsMHoiIGZpbGw9IiMwMDAwMDAiLz4KCTxwYXRoIGQ9Ik0yOTMuOTA5LDE4Ny4yMTVsLTExMS44MTgtNzMuNTkxQzE2Mi43OTIsMTAwLjkyNiwxNDcsMTA5LjQ0NSwxNDcsMTMyLjU0NVYyODcuNDJjMCwyMy4xLDE1LjgxMywzMS42NDcsMzUuMTQ3LDE4Ljk5OCAgIEwyOTMuODYsMjMzLjMxQzMxMy4xODcsMjIwLjY0NywzMTMuMjA4LDE5OS45MTMsMjkzLjkwOSwxODcuMjE1eiBNMjc5LjAwNiwyMTcuODY4bC05OS4yOTUsNjQuOTgxICAgYy02LjQ0LDQuMjIxLTExLjcxMSwxLjM3Mi0xMS43MTEtNi4zMjhWMTQzLjQzN2MwLTcuNyw1LjI2NC0xMC41MzUsMTEuNjk3LTYuM2w5OS4zMyw2NS4zNjYgICBDMjg1LjQ2LDIwNi43MzEsMjg1LjQ1MywyMTMuNjQ3LDI3OS4wMDYsMjE3Ljg2OHoiIGZpbGw9IiMwMDAwMDAiLz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8L3N2Zz4K');
  }

  .narrator-sections-center-pause {
    position: absolute;
    top: -128px;
    left: -128px;
    width: 256px;
    height: 256px;
    -webkit-transition: opacity .5s linear;
    -moz-transition: opacity .5s linear;
    -o-transition: opacity .5s linear;
    transition: opacity .5s linear;
    background-size: 256px 256px;
    background: url('data:image/svg+xml;utf8;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pgo8IS0tIEdlbmVyYXRvcjogQWRvYmUgSWxsdXN0cmF0b3IgMTkuMC4wLCBTVkcgRXhwb3J0IFBsdWctSW4gLiBTVkcgVmVyc2lvbjogNi4wMCBCdWlsZCAwKSAgLS0+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmVyc2lvbj0iMS4xIiBpZD0iQ2FwYV8xIiB4PSIwcHgiIHk9IjBweCIgdmlld0JveD0iMCAwIDQyMCA0MjAiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDQyMCA0MjA7IiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iMjU2cHgiIGhlaWdodD0iMjU2cHgiPgo8Zz4KCTxwYXRoIGQ9Ik0yMTAsMjFjMTA0LjIxNiwwLDE4OSw4NC43ODQsMTg5LDE4OXMtODQuNzg0LDE4OS0xODksMTg5UzIxLDMxNC4yMTYsMjEsMjEwUzEwNS43ODQsMjEsMjEwLDIxIE0yMTAsMCAgIEM5NC4wMzEsMCwwLDk0LjAyNCwwLDIxMHM5NC4wMzEsMjEwLDIxMCwyMTBzMjEwLTk0LjAyNCwyMTAtMjEwUzMyNS45NjksMCwyMTAsMEwyMTAsMHoiIGZpbGw9IiMwMDAwMDAiLz4KCTxnPgoJCTxwYXRoIGQ9Ik0yNTksMTA4Ljk0MWMtMTkuMjUsMC0zNSwxNS43NS0zNSwzNXYxMzIuMTI1YzAsMTkuMjUsMTUuNzUsMzUsMzUsMzVzMzUtMTUuNzUsMzUtMzVWMTQzLjk0MSAgICBDMjk0LDEyNC42OTEsMjc4LjI1LDEwOC45NDEsMjU5LDEwOC45NDF6IE0yNzMsMjc2LjA2NmMwLDcuNy02LjMsMTQtMTQsMTRzLTE0LTYuMy0xNC0xNFYxNDMuOTQxYzAtNy43LDYuMy0xNCwxNC0xNCAgICBzMTQsNi4zLDE0LDE0VjI3Ni4wNjZ6IiBmaWxsPSIjMDAwMDAwIi8+CgkJPHBhdGggZD0iTTE2MSwxMDguOTQxYy0xOS4yNSwwLTM1LDE1Ljc1LTM1LDM1djEzMi4xMjVjMCwxOS4yNSwxNS43NSwzNSwzNSwzNXMzNS0xNS43NSwzNS0zNVYxNDMuOTQxICAgIEMxOTYsMTI0LjY5MSwxODAuMjUsMTA4Ljk0MSwxNjEsMTA4Ljk0MXogTTE3NSwyNzYuMDY2YzAsNy43LTYuMywxNC0xNCwxNHMtMTQtNi4zLTE0LTE0VjE0My45NDFjMC03LjcsNi4zLTE0LDE0LTE0ICAgIHMxNCw2LjMsMTQsMTRWMjc2LjA2NnoiIGZpbGw9IiMwMDAwMDAiLz4KCTwvZz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8Zz4KPC9nPgo8L3N2Zz4K');
  }

  .narrator-section {
    -webkit-transition: font-size .5s ease-in-out;
    -moz-transition: font-size .5s ease-in-out;
    -o-transition: font-size .5s ease-in-out;
    transition: font-size .5s ease-in-out;
  }

  .narrator-section.narrator-current {
    font-size: " font-max ";
  }

  .narrator-section:not(.narrator-current) {
    font-size: " font-min ";
  }

  "))
