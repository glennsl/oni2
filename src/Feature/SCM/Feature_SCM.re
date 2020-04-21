open Oni_Core;
open Utility;

module ContextMenu = Oni_Components.ContextMenu;
module InputModel = Oni_Components.InputModel;
module ExtHostClient = Oni_Extensions.ExtHostClient;
module Selection = Oni_Components.Selection;

// MODEL

[@deriving show]
type command = ExtHostClient.SCM.command;

module Resource = ExtHostClient.SCM.Resource;
module ResourceGroup = ExtHostClient.SCM.ResourceGroup;
module Provider = ExtHostClient.SCM.Provider;

type menu =
  | GroupMenu({handle: int})
  | ResourceMenu({
      group: int,
      resource: int,
    });

[@deriving show({with_path: false})]
type model = {
  providers: list(Provider.t),
  inputBox,
}

and inputBox = {
  value: string,
  selection: Selection.t,
  placeholder: string,
};

let initial = {
  providers: [],
  inputBox: {
    value: "",
    selection: Selection.initial,
    placeholder: "Do the commit thing!",
  },
};

// EFFECTS

module Effects = {
  let getOriginalUri = (extHostClient, model, path, toMsg) =>
    ExtHostClient.SCM.Effects.provideOriginalResource(
      extHostClient,
      model.providers,
      path,
      toMsg,
    );
};

// UPDATE

[@deriving show({with_path: false})]
type msg =
  | NewProvider({
      handle: int,
      id: string,
      label: string,
      rootUri: option(Uri.t),
    })
  | LostProvider({handle: int})
  | NewResourceGroup({
      provider: int,
      handle: int,
      id: string,
      label: string,
    })
  | LostResourceGroup({
      provider: int,
      handle: int,
    })
  | ResourceStatesChanged({
      provider: int,
      group: int,
      spliceStart: int,
      deleteCount: int,
      additions: list(Resource.t),
    })
  | CountChanged({
      handle: int,
      count: int,
    })
  | QuickDiffProviderChanged({
      handle: int,
      available: bool,
    })
  | CommitTemplateChanged({
      handle: int,
      template: string,
    })
  | AcceptInputCommandChanged({
      handle: int,
      command,
    })
  | KeyPressed({key: string})
  | InputBoxClicked({selection: Selection.t})
  | GroupRightClick({handle: int})
  | ResourceRightClick({
      group: int,
      resource: int,
    })
  | GroupMenuItemSelected({
      handle: int,
      item: Menu.item,
    })
  | ResourceMenuItemSelected({
      uri: Uri.t,
      item: Menu.item,
    });

module Msg = {
  let keyPressed = key => KeyPressed({key: key});
};

type outmsg =
  | Effect(Isolinear.Effect.t(msg))
  | Focus
  | ContextMenu(menu)
  | MenuItemSelected({
      command: string,
      argument: Json.t,
    })
  | Nothing;

let update = (extHostClient, model, msg) =>
  switch (msg) {
  | NewProvider({handle, id, label, rootUri}) => (
      {
        ...model,
        providers: [
          Provider.{
            handle,
            id,
            label,
            rootUri,
            resourceGroups: [],
            hasQuickDiffProvider: false,
            count: 0,
            commitTemplate: "",
            acceptInputCommand: None,
          },
          ...model.providers,
        ],
      },
      Nothing,
    )

  | LostProvider({handle}) => (
      {
        ...model,
        providers:
          List.filter(
            (it: Provider.t) => it.handle != handle,
            model.providers,
          ),
      },
      Nothing,
    )

  | QuickDiffProviderChanged({handle, available}) => (
      {
        ...model,
        providers:
          List.map(
            (it: Provider.t) =>
              it.handle == handle
                ? {...it, hasQuickDiffProvider: available} : it,
            model.providers,
          ),
      },
      Nothing,
    )

  | CountChanged({handle, count}) => (
      {
        ...model,
        providers:
          List.map(
            (it: Provider.t) => it.handle == handle ? {...it, count} : it,
            model.providers,
          ),
      },
      Nothing,
    )

  | CommitTemplateChanged({handle, template}) => (
      {
        ...model,
        providers:
          List.map(
            (it: Provider.t) =>
              it.handle == handle ? {...it, commitTemplate: template} : it,
            model.providers,
          ),
      },
      Nothing,
    )

  | AcceptInputCommandChanged({handle, command}) => (
      {
        ...model,
        providers:
          List.map(
            (it: Provider.t) =>
              it.handle == handle
                ? {...it, acceptInputCommand: Some(command)} : it,
            model.providers,
          ),
      },
      Nothing,
    )

  | NewResourceGroup({provider, handle, id, label}) => (
      {
        ...model,
        providers:
          List.map(
            (p: Provider.t) =>
              p.handle == provider
                ? {
                  ...p,
                  resourceGroups: [
                    ResourceGroup.{
                      handle,
                      id,
                      label,
                      hideWhenEmpty: false,
                      resources: [],
                    },
                    ...p.resourceGroups,
                  ],
                }
                : p,
            model.providers,
          ),
      },
      Nothing,
    )

  | LostResourceGroup({provider, handle}) => (
      {
        ...model,
        providers:
          List.map(
            (p: Provider.t) =>
              p.handle == provider
                ? {
                  ...p,
                  resourceGroups:
                    List.filter(
                      (g: ResourceGroup.t) => g.handle != handle,
                      p.resourceGroups,
                    ),
                }
                : p,
            model.providers,
          ),
      },
      Nothing,
    )

  | ResourceStatesChanged({
      provider,
      group,
      spliceStart,
      deleteCount,
      additions,
    }) => (
      {
        ...model,
        providers:
          List.map(
            (p: Provider.t) =>
              p.handle == provider
                ? {
                  ...p,
                  resourceGroups:
                    List.map(
                      (g: ResourceGroup.t) =>
                        g.handle == group
                          ? {
                            ...g,
                            resources:
                              ListEx.splice(
                                ~start=spliceStart,
                                ~deleteCount,
                                ~additions,
                                g.resources,
                              ),
                          }
                          : g,
                      p.resourceGroups,
                    ),
                }
                : p,
            model.providers,
          ),
      },
      Nothing,
    )

  | KeyPressed({key: "<CR>"}) => (
      model,
      Effect(
        Isolinear.Effect.batch(
          model.providers
          |> List.map((provider: Provider.t) =>
               switch (provider.acceptInputCommand) {
               | Some(command) =>
                 ExtHostClient.Effects.executeContributedCommand(
                   extHostClient,
                   command.id,
                   ~arguments=command.arguments,
                 )
               | None => Isolinear.Effect.none
               }
             ),
        ),
      ),
    )

  | KeyPressed({key}) =>
    let (value, selection) =
      InputModel.handleInput(
        ~text=model.inputBox.value,
        ~selection=model.inputBox.selection,
        key,
      );

    (
      {
        ...model,
        inputBox: {
          ...model.inputBox,
          value,
          selection,
        },
      },
      Effect(
        Isolinear.Effect.batch(
          model.providers
          |> List.map(provider =>
               ExtHostClient.SCM.Effects.onInputBoxValueChange(
                 extHostClient,
                 provider,
                 value,
               )
             ),
        ),
      ),
    );

  | InputBoxClicked({selection}) => (
      {
        ...model,
        inputBox: {
          ...model.inputBox,
          selection,
        },
      },
      Focus,
    )

  | GroupRightClick({handle}) => (
      model,
      ContextMenu(GroupMenu({handle: handle})),
    )

  | ResourceRightClick({group, resource}) => (
      model,
      ContextMenu(ResourceMenu({group, resource})),
    )

  | GroupMenuItemSelected({handle, item}) => (
      model,
      MenuItemSelected({
        command: item.command,
        argument: Json.Encode.int(handle),
      }),
    )

  | ResourceMenuItemSelected({uri, item}) => (
      model,
      MenuItemSelected({
        command: item.command,
        argument: Uri.to_yojson(uri),
      }),
    )
  };

let handleExtensionMessage = (~dispatch, msg: ExtHostClient.SCM.msg) =>
  switch (msg) {
  | RegisterSourceControl({handle, id, label, rootUri}) =>
    dispatch(NewProvider({handle, id, label, rootUri}))

  | UnregisterSourceControl({handle}) =>
    dispatch(LostProvider({handle: handle}))

  | RegisterSCMResourceGroup({provider, handle, id, label}) =>
    dispatch(NewResourceGroup({provider, handle, id, label}))

  | UnregisterSCMResourceGroup({provider, handle}) =>
    dispatch(LostResourceGroup({provider, handle}))

  | SpliceSCMResourceStates({provider, group, start, deleteCount, additions}) =>
    dispatch(
      ResourceStatesChanged({
        provider,
        group,
        spliceStart: start,
        deleteCount,
        additions,
      }),
    )

  | UpdateSourceControl({
      handle,
      hasQuickDiffProvider,
      count,
      commitTemplate,
      acceptInputCommand,
    }) =>
    Option.iter(
      available => dispatch(QuickDiffProviderChanged({handle, available})),
      hasQuickDiffProvider,
    );
    Option.iter(count => dispatch(CountChanged({handle, count})), count);
    Option.iter(
      template => dispatch(CommitTemplateChanged({handle, template})),
      commitTemplate,
    );
    Option.iter(
      command => dispatch(AcceptInputCommandChanged({handle, command})),
      acceptInputCommand,
    );
  };

// VIEW

open Revery;
open Revery.UI;
open Revery.UI.Components;

module Input = Oni_Components.Input;

module Colors = Feature_Theme.Colors;

module Pane = {
  module Styles = {
    open Style;

    let container = [padding(10), flexGrow(1)];

    let text = (~theme, ~font: UiFont.t) => [
      fontSize(font.fontSize),
      fontFamily(font.fontFile),
      color(Colors.SideBar.foreground.from(theme)),
      textWrap(TextWrapping.NoWrap),
      textOverflow(`Ellipsis),
    ];

    let input = (~font: UiFont.t) => [
      fontFamily(font.fontFile),
      fontSize(font.fontSize),
      flexGrow(1),
    ];

    let group = [];

    let groupLabel = [paddingVertical(3)];

    let groupLabelText = (~theme, ~font: UiFont.t) => [
      fontSize(font.fontSize *. 0.85),
      fontFamily(font.fontFileBold),
      color(Colors.SideBar.foreground.from(theme)),
      textWrap(TextWrapping.NoWrap),
      textOverflow(`Ellipsis),
    ];

    let groupItems = [marginLeft(6)];

    let item = (~isHovered, ~theme) => [
      isHovered
        ? backgroundColor(Colors.List.hoverBackground.from(theme))
        : backgroundColor(Colors.SideBar.background.from(theme)),
      paddingVertical(2),
      cursor(MouseCursors.pointer),
    ];
  };

  let component = React.Expert.component("groupView");
  let itemView =
      (
        ~provider: Provider.t,
        ~group: ResourceGroup.t,
        ~resource: Resource.t,
        ~workingDirectory: option(string),
        ~onClick,
        ~menus,
        ~currentMenu,
        ~theme,
        ~font,
        ~dispatch,
        (),
      ) =>
    component(hooks => {
      open Base;
      let ((isHovered, setHovered), hooks) = Hooks.state(false, hooks);
      let onMouseOver = _ => setHovered(_ => true);
      let onMouseOut = _ => setHovered(_ => false);
      let isMenuOpen =
        Poly.(
          currentMenu
          == Some(
               ResourceMenu({group: group.handle, resource: resource.handle}),
             )
        );
      let onRightClick = () =>
        dispatch(
          ResourceRightClick({
            group: group.handle,
            resource: resource.handle,
          }),
        );

      let base =
        Option.first_some(
          Option.map(provider.rootUri, ~f=Uri.toFileSystemPath),
          workingDirectory,
        )
        |> Option.value(~default="/");

      let path = Uri.toFileSystemPath(resource.uri);
      let displayName = Path.toRelative(~base, path);

      let menu = () =>
        if (isMenuOpen) {
          <ContextMenu
            items={
              Feature_Menus.scmResourceStateContext(menus)
              |> List.map(~f=(item: Menu.item) =>
                   ContextMenu.{label: item.label, data: item}
                 )
            }
            onItemSelect={item => {
              dispatch(
                ResourceMenuItemSelected({
                  uri: resource.uri,
                  item: item.data,
                }),
              )
            }}
            theme
            font
          />;
        } else {
          React.empty;
        };

      (
        <View style={Styles.item(~isHovered, ~theme)} onMouseOver onMouseOut>
          <Clickable onClick onRightClick>
            <Text style={Styles.text(~font, ~theme)} text=displayName />
          </Clickable>
          <menu />
        </View>,
        hooks,
      );
    });

  let groupView =
      (
        ~provider: Provider.t,
        ~group: ResourceGroup.t,
        ~workingDirectory,
        ~onItemClick,
        ~menus,
        ~currentMenu,
        ~contextKeys,
        ~theme,
        ~font,
        ~dispatch,
        (),
      ) => {
    let isMenuOpen = currentMenu == Some(GroupMenu({handle: group.handle}));
    let onRightClick = () =>
      dispatch(GroupRightClick({handle: group.handle}));

    let label = String.uppercase_ascii(group.label);

    let contextKeys =
      WhenExpr.ContextKeys.union(
        contextKeys,
        WhenExpr.ContextKeys.fromList([
          ("scmProvider", WhenExpr.Value.String(provider.id)),
          ("scmResourceGroup", WhenExpr.Value.String(group.id)),
        ]),
      );

    let menu = () =>
      if (isMenuOpen) {
        <ContextMenu
          items={
            Feature_Menus.scmResourceGroupContext(menus)
            |> List.filter((item: Menu.item) =>
                 item.group != Some("inline")
                 && WhenExpr.evaluate(
                      item.isVisibleWhen,
                      WhenExpr.ContextKeys.getValue(contextKeys),
                    )
               )
            |> List.map((item: Menu.item) =>
                 ContextMenu.{label: item.label, data: item}
               )
          }
          onItemSelect={item => {
            dispatch(
              GroupMenuItemSelected({handle: group.handle, item: item.data}),
            )
          }}
          theme
          font
        />;
      } else {
        React.empty;
      };

    <View style=Styles.group>
      <Clickable style=Styles.groupLabel onRightClick>
        <Text style={Styles.groupLabelText(~font, ~theme)} text=label />
      </Clickable>
      <menu />
      <View style=Styles.groupItems>
        ...{
             group.resources
             |> List.map(resource =>
                  <itemView
                    provider
                    group
                    resource
                    workingDirectory
                    onClick={() => onItemClick(resource)}
                    menus
                    currentMenu
                    theme
                    font
                    dispatch
                  />
                )
             |> React.listToElement
           }
      </View>
    </View>;
  };

  let make =
      (
        ~model,
        ~workingDirectory,
        ~onItemClick,
        ~isFocused,
        ~menus,
        ~currentMenu,
        ~contextKeys,
        ~theme,
        ~font,
        ~dispatch,
        (),
      ) => {
    let groups = {
      open Base.List.Let_syntax;

      let%bind provider = model.providers;
      let%bind group = provider.resourceGroups;

      return((provider, group));
    };

    <ScrollView style=Styles.container>
      <Input
        style={Styles.input(~font)}
        value={model.inputBox.value}
        selection={model.inputBox.selection}
        placeholder={model.inputBox.placeholder}
        isFocused
        onClick={selection =>
          dispatch(InputBoxClicked({selection: selection}))
        }
        theme
      />
      {groups
       |> List.map(((provider, group)) =>
            <groupView
              provider
              group
              workingDirectory
              onItemClick
              menus
              currentMenu
              contextKeys
              theme
              font
              dispatch
            />
          )
       |> React.listToElement}
    </ScrollView>;
  };
};
