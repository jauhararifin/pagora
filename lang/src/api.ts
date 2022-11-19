import { FunctionType, TypeKind } from './semantic'

export interface BuiltinAPIs {
  [funcname: string]: FunctionType
}

export const apis: BuiltinAPIs = {
  draw_rect: {
    kind: TypeKind.FUNCTION,
    arguments: [
      { kind: TypeKind.INTEGER }, // x
      { kind: TypeKind.INTEGER }, // y
      { kind: TypeKind.INTEGER }, // width
      { kind: TypeKind.INTEGER }, // height
      { kind: TypeKind.STRING }, // stroke color like #000
      { kind: TypeKind.STRING }, // bg color like #000
    ],
    return: { kind: TypeKind.VOID },
  },
  get_width: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER }, // width
  },
  get_height: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER }, // height
  },
  output: {
    kind: TypeKind.FUNCTION,
    arguments: [{ kind: TypeKind.STRING }],
    return: { kind: TypeKind.VOID },
  },
  register_on_update: {
    kind: TypeKind.FUNCTION,
    arguments: [
      {
        kind: TypeKind.FUNCTION,
        arguments: [],
        return: { kind: TypeKind.VOID },
      },
    ],
    return: { kind: TypeKind.VOID },
  },
  register_on_keydown: {
    kind: TypeKind.FUNCTION,
    arguments: [
      {
        kind: TypeKind.FUNCTION,
        arguments: [{ kind: TypeKind.STRING }],
        return: { kind: TypeKind.VOID },
      },
    ],
    return: { kind: TypeKind.VOID },
  },
  register_on_mouse_click: {
    kind: TypeKind.FUNCTION,
    arguments: [
      {
        kind: TypeKind.FUNCTION,
        arguments: [{ kind: TypeKind.INTEGER }, { kind: TypeKind.INTEGER }],
        return: { kind: TypeKind.VOID },
      },
    ],
    return: { kind: TypeKind.VOID },
  },
  register_on_mouse_move: {
    kind: TypeKind.FUNCTION,
    arguments: [
      {
        kind: TypeKind.FUNCTION,
        arguments: [{ kind: TypeKind.INTEGER }, { kind: TypeKind.INTEGER }],
        return: { kind: TypeKind.VOID },
      },
    ],
    return: { kind: TypeKind.VOID },
  },
  unix_time_millis: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER },
  },
  system_time_millis: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER },
  },
  // TODO: add function to registerEvent like `onMouseClick(functionName)`
  // TODO: add function to registerEvent like `onResize(functionName)`
  // TODO: add function to registerEvent like `onDraw(functionName)`
  // TODO: add function to registerEvent like `onTick(functionName)`
}
