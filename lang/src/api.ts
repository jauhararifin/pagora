import { FunctionType, TypeKind } from './semantic'

export type BuiltinAPIs = { [funcname: string]: FunctionType }

export const apis: BuiltinAPIs = {
  draw_pixel: {
    kind: TypeKind.FUNCTION,
    arguments: [
      { kind: TypeKind.INTEGER }, // x
      { kind: TypeKind.INTEGER }, // y
      { kind: TypeKind.STRING }, // color like #000
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
