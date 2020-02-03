open EditorCoreTypes;
open Revery.UI;

open Oni_Core;

open Helpers;

module Log = (val Log.withNamespace("Oni2.Editor.SurfaceView"));

module Styles = {
  open Style;

  let bufferViewClipped = (offsetLeft, bufferPixelWidth) => [
    overflow(`Hidden),
    position(`Absolute),
    top(0),
    left(int_of_float(offsetLeft)),
    width(int_of_float(bufferPixelWidth)),
    bottom(0),
  ];
};

let drawCurrentLineHighlight =
    (~transform, ~metrics, ~scrollY, ~lineHeight, ~theme: Theme.t, line) =>
  DrawPrimitives.drawLineHighlight(
    ~transform,
    ~metrics,
    ~scrollY,
    ~lineHeight,
    ~color=theme.editorLineHighlightBackground,
    line,
  );

let renderRulers =
    (
      ~rulers,
      ~scrollX,
      ~scrollY,
      ~editorFont,
      ~transform,
      ~metrics,
      ~theme: Theme.t,
    ) =>
  rulers
  |> List.map(bufferPositionToPixel(~scrollX, ~scrollY, ~editorFont, 0))
  |> List.map(fst)
  |> List.iter(
       DrawPrimitives.drawRuler(
         ~transform,
         ~metrics,
         ~color=theme.editorRulerForeground,
       ),
     );

let%component make =
              (
                ~onScroll,
                ~buffer,
                ~editor,
                ~metrics,
                ~theme,
                ~topVisibleLine,
                ~onCursorChange,
                ~layout: EditorLayout.t,
                ~cursorPosition: Location.t,
                ~rulers,
                ~editorFont: EditorFont.t,
                ~leftVisibleColumn,
                ~diagnosticsMap,
                ~selectionRanges,
                ~matchingPairs,
                ~bufferHighlights,
                ~definition,
                ~bufferSyntaxHighlights,
                ~shouldRenderWhitespace,
                ~shouldRenderIndentGuides,
                ~bottomVisibleLine,
                ~shouldHighlightActiveIndentGuides,
                ~mode,
                ~isActiveSplit,
                ~gutterWidth,
                (),
              ) => {
  let%hook (elementRef, setElementRef) = React.Hooks.ref(None);

  let Editor.{scrollX, scrollY, _} = editor;
  let bufferPixelWidth =
    layout.lineNumberWidthInPixels +. layout.bufferWidthInPixels;
  let lineCount = Buffer.getNumberOfLines(buffer);
  let indentation =
    switch (Buffer.getIndentation(buffer)) {
    | Some(v) => v
    | None => IndentationSettings.default
    };

  let onMouseWheel = (wheelEvent: NodeEvents.mouseWheelEventParams) =>
    onScroll(wheelEvent.deltaY *. (-50.));

  let onMouseUp = (evt: NodeEvents.mouseButtonEventParams) => {
    Log.trace("editorMouseUp");

    switch (elementRef) {
    | None => ()
    | Some(r) =>
      let rect = r#getBoundingBox() |> Revery.Math.Rectangle.ofBoundingBox;

      let relY = evt.mouseY -. Revery.Math.Rectangle.getY(rect);
      let relX = evt.mouseX -. Revery.Math.Rectangle.getX(rect);

      let numberOfLines = Buffer.getNumberOfLines(buffer);
      let (line, col) =
        Editor.pixelPositionToLineColumn(editor, metrics, relX, relY);

      if (line < numberOfLines) {
        Log.tracef(m => m("  topVisibleLine is %i", topVisibleLine));
        Log.tracef(m => m("  setPosition (%i, %i)", line + 1, col));

        let cursor =
          Vim.Cursor.create(
            ~line=Index.fromOneBased(line + 1),
            ~column=Index.fromZeroBased(col),
          );

        /*GlobalContext.current().dispatch(
            Actions.EditorScrollToLine(editorId, topVisibleLine),
          );
          GlobalContext.current().dispatch(
            Actions.EditorScrollToColumn(editorId, leftVisibleColumn),
          );*/
        onCursorChange(cursor);
      };
    };
  };

  let horizontalScrollBarStyle =
    Style.[
      position(`Absolute),
      bottom(0),
      left(int_of_float(layout.lineNumberWidthInPixels)),
      height(Constants.default.scrollBarThickness),
      width(int_of_float(layout.bufferWidthInPixels)),
    ];

  <View
    ref={node => setElementRef(Some(node))}
    style={Styles.bufferViewClipped(
      gutterWidth,
      bufferPixelWidth -. gutterWidth,
    )}
    onMouseUp
    onMouseWheel>
    <OpenGL
      style={Styles.bufferViewClipped(0., bufferPixelWidth -. gutterWidth)}
      render={(transform, _ctx) => {
        let count = lineCount;
        let height = metrics.pixelHeight;
        let rowHeight = metrics.lineHeight;

        drawCurrentLineHighlight(
          cursorPosition.line,
          ~transform,
          ~metrics,
          ~scrollY,
          ~lineHeight={editorFont.measuredHeight},
          ~theme,
        );

        renderRulers(
          ~rulers,
          ~editorFont,
          ~scrollX,
          ~scrollY,
          ~transform,
          ~metrics,
          ~theme,
        );

        let bufferPositionToPixel =
          bufferPositionToPixel(~scrollX, ~scrollY, ~editorFont);

        ContentView.render(
          ~scrollY,
          ~rowHeight,
          ~height,
          ~count,
          ~transform,
          ~buffer,
          ~editor,
          ~leftVisibleColumn,
          ~theme,
          ~diagnosticsMap,
          ~selectionRanges,
          ~matchingPairs,
          ~bufferHighlights,
          ~cursorPosition,
          ~definition,
          ~layout,
          ~bufferSyntaxHighlights,
          ~editorFont,
          ~shouldRenderWhitespace,
        );

        if (shouldRenderIndentGuides) {
          IndentLineRenderer.render(
            ~transform,
            ~buffer,
            ~startLine=topVisibleLine - 1,
            ~endLine=bottomVisibleLine + 1,
            ~lineHeight=editorFont.measuredHeight,
            ~fontWidth=editorFont.measuredWidth,
            ~cursorLine=Index.toZeroBased(cursorPosition.line),
            ~theme,
            ~indentationSettings=indentation,
            ~bufferPositionToPixel,
            ~showActive=shouldHighlightActiveIndentGuides,
            (),
          );
        };
      }}
    />
    <CursorView buffer mode isActiveSplit cursorPosition editor editorFont />
    <View style=horizontalScrollBarStyle>
      <EditorHorizontalScrollbar
        editor
        metrics
        width={int_of_float(layout.bufferWidthInPixels)}
        theme
      />
    </View>
  </View>;
};
