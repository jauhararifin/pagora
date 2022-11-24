import { Function, TypeKind } from './semantic'

export interface BuiltinAPIs {
  functions: Function[]
}

const drawRect: Function = {
  name: 'draw_rect',
  type: {
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
  arguments: ['x', 'y', 'width', 'height', 'stroke', 'fill'],
}

const getWidth: Function = {
  name: 'get_width',
  type: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER },
  },
  arguments: ['width'],
}

const getHeight: Function = {
  name: 'get_height',
  type: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER },
  },
  arguments: ['height'],
}

const output: Function = {
  name: 'output',
  type: {
    kind: TypeKind.FUNCTION,
    arguments: [{ kind: TypeKind.STRING }],
    return: { kind: TypeKind.VOID },
  },
  arguments: ['message'],
}

const registerOnUpdate: Function = {
  name: 'register_on_update',
  type: {
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
  arguments: ['callback'],
}

const registerOnKeydown: Function = {
  name: 'register_on_keydown',
  type: {
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
  arguments: ['callback'],
}

const registerOnClick: Function = {
  name: 'register_on_mouse_click',
  type: {
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
  arguments: ['callback'],
}

const registerOnMouseMove: Function = {
  name: 'register_on_mouse_move',
  type: {
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
  arguments: ['callback'],
}

const unixTimeMillis: Function = {
  name: 'unix_time_millis',
  type: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER },
  },
  arguments: [],
}

const systemTimeMillis: Function = {
  name: 'system_time_millis',
  type: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER },
  },
  arguments: [],
}

export const apis: BuiltinAPIs = {
  functions: [
    drawRect,
    getWidth,
    getHeight,
    output,
    registerOnUpdate,
    registerOnKeydown,
    registerOnClick,
    registerOnMouseMove,
    unixTimeMillis,
    systemTimeMillis,
  ],
}
