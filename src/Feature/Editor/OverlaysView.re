open EditorCoreTypes;
open Revery.UI;

open Oni_Core;

module Log = (val Log.withNamespace("Oni2.UI.EditorSurface"));

module Option = Utility.Option;
module FontIcon = Oni_Components.FontIcon;
module BufferHighlights = Oni_Syntax.BufferHighlights;
module Completions = Feature_LanguageSupport.Completions;
module Diagnostics = Feature_LanguageSupport.Diagnostics;
module Diagnostic = Feature_LanguageSupport.Diagnostic;
module Definition = Feature_LanguageSupport.Definition;

module Styles = {
  open Style;

  let bufferViewOverlay = bufferPixelWidth => [
    pointerEvents(`Ignore),
    position(`Absolute),
    top(0),
    left(0),
    width(int_of_float(bufferPixelWidth)),
    bottom(0),
  ];
};

let completionsView =
    (
      ~completions,
      ~cursorPixelX,
      ~cursorPixelY,
      ~theme,
      ~tokenTheme,
      ~editorFont: EditorFont.t,
      (),
    ) =>
  Completions.isActive(completions)
    ? <CompletionsView
        x=cursorPixelX
        y=cursorPixelY
        lineHeight={editorFont.measuredHeight}
        theme
        tokenTheme
        editorFont
        completions
      />
    : React.empty;

let make =
    (
      ~buffer,
      ~isActiveSplit,
      ~hoverDelay,
      ~isHoverEnabled,
      ~diagnostics,
      ~mode,
      ~layout: EditorLayout.t,
      ~cursorPosition: Location.t,
      ~editor: Editor.t,
      ~gutterWidth,
      ~completions,
      ~theme,
      ~tokenTheme,
      ~editorFont: EditorFont.t,
      (),
    ) => {
  let cursorLine = Index.toZeroBased(cursorPosition.line);
  let lineCount = Buffer.getNumberOfLines(buffer);

  let bufferPixelWidth =
    layout.lineNumberWidthInPixels +. layout.bufferWidthInPixels;

  let (cursorOffset, _cursorCharacterWidth) =
    if (lineCount > 0 && cursorLine < lineCount) {
      let cursorLine = Buffer.getLine(cursorLine, buffer);

      let (cursorOffset, width) =
        BufferViewTokenizer.getCharacterPositionAndWidth(
          cursorLine,
          Index.toZeroBased(cursorPosition.column),
        );
      (cursorOffset, width);
    } else {
      (0, 1);
    };

  let cursorPixelY =
    int_of_float(
      editorFont.measuredHeight
      *. float(Index.toZeroBased(cursorPosition.line))
      -. editor.scrollY
      +. 0.5,
    );

  let cursorPixelX =
    int_of_float(
      gutterWidth
      +. editorFont.measuredWidth
      *. float(cursorOffset)
      -. editor.scrollX
      +. 0.5,
    );

  isActiveSplit
    ? <View style={Styles.bufferViewOverlay(bufferPixelWidth)}>
        <HoverView
          x=cursorPixelX
          y=cursorPixelY
          delay=hoverDelay
          isEnabled=isHoverEnabled
          theme
          editorFont
          diagnostics
          editor
          buffer
          mode
        />
        <completionsView
          completions
          cursorPixelX
          cursorPixelY
          theme
          tokenTheme
          editorFont
        />
      </View>
    : React.empty;
};