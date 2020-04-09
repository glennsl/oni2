open Revery.UI;

open Oni_Model;
module Core = Oni_Core;

open Oni_Model.SideBar;

module Colors = Feature_Theme.Colors;

module Styles = {
  open Style;

  let container = (~theme, ~transition) =>
    Style.[
      backgroundColor(Colors.SideBar.background.from(theme)),
      width(225),
      transform(Transform.[TranslateX(transition)]),
    ];

  let title = (~theme, ~font: Core.UiFont.t) => [
    fontSize(font.fontSize),
    fontFamily(font.fontFileSemiBold),
    color(Colors.SideBar.foreground.from(theme)),
  ];

  let heading = theme => [
    flexDirection(`Row),
    justifyContent(`Center),
    alignItems(`Center),
    backgroundColor(Colors.SideBar.background.from(theme)),
    height(Core.Constants.tabHeight),
  ];
};

let animation =
  Revery.UI.Animation.(
    animate(Revery.Time.milliseconds(150))
    |> ease(Easing.easeIn)
    |> tween(-50.0, 0.)
    |> delay(Revery.Time.milliseconds(0))
  );

let getMenu = (id, extensions) => {
  let commands =
    List.fold_left((acc, extension: Oni_Extensions.ExtensionScanner.t) =>
      List.fold_left((acc, cmd: Oni_Extensions.ExtensionContributions.Command.t) =>
        Core.StringMap.add(cmd.command, cmd, acc),
        Core.StringMap.empty,
        extension.manifest.contributes.commands,
      )
      |> Core.StringMap.union((_, _, x) => Some(x), acc),
      Core.StringMap.empty,
      extensions
    );

  let menus =
    List.fold_left((acc, extension: Oni_Extensions.ExtensionScanner.t) =>
      List.fold_left((acc, menu: Oni_Extensions.ExtensionContributions.Menu.t) =>
        Core.StringMap.add(menu.id, menu.items, acc),
        Core.StringMap.empty,
        extension.manifest.contributes.menus,
      )
      |> Core.StringMap.union((_, xs, ys) => Some(List.append(xs, ys)), acc),
      Core.StringMap.empty,
      extensions
    );

  menus
  |> Core.StringMap.find_opt(id)
  |> Option.value(~default=[])
  |> List.filter_map((item: Oni_Extensions.ExtensionContributions.Menu.item) => {
    commands
    |> Core.StringMap.find_opt(item.command)
    |> Option.map((cmd: Oni_Extensions.ExtensionContributions.Command.t) => (Oni_Extensions.LocalizedToken.to_string(cmd.title), Actions.Command(cmd.command)))
  });
};

let%component make = (~theme, ~state: State.t, ()) => {
  [@warning "-27"]
  let State.{sideBar, uiFont: font, _} = state;

  let%hook (transition, _animationState, _reset) =
    Hooks.animation(animation, ~active=true);

  let title =
    switch (sideBar.selected) {
    | FileExplorer => "Explorer"
    | SCM => "Source Control"
    | Extensions => "Extensions"
    };

  let elem =
    switch (sideBar.selected) {
    | FileExplorer =>
      <FileExplorerView model={state.fileExplorer} theme font />

    | SCM =>
      let onItemClick = (resource: Feature_SCM.Resource.t) =>
        GlobalContext.current().dispatch(
          Actions.OpenFileByPath(
            Oni_Core.Uri.toFileSystemPath(resource.uri),
            None,
            None,
          ),
        );

      let workingDirectory =
        Option.map(w => w.Workspace.workingDirectory, state.workspace);

      <Feature_SCM.Pane
        model={state.scm}
        workingDirectory
        onItemClick
        isFocused={FocusManager.current(state) == Focus.SCM}
        groupMenu=getMenu("scm/resourceGroup/context", state.extensions.extensions)
        resourceMenu=getMenu("scm/resourceState/context", state.extensions.extensions)
        // resourceMenu=[("test", Actions.Noop), ("foo", Actions.Noop)]
        onSelectMenuItem=GlobalContext.current().dispatch
        currentMenu={
          switch (state.contextMenu) {
          | SCM(menu) => Some(menu)
          | _ => None
          }
        }
        theme
        font
        dispatch={msg => GlobalContext.current().dispatch(Actions.SCM(msg))}
      />;

    | Extensions => <ExtensionListView model={state.extensions} theme font />
    };

  <View style={Styles.container(~theme, ~transition)}>
    <View style={Styles.heading(theme)}>
      <Text text=title style={Styles.title(~theme, ~font)} />
    </View>
    elem
  </View>;
};
