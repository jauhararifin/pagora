import ReactCodeMirror from "@uiw/react-codemirror";
import { FaCaretDown, FaCaretRight, FaClock, FaSquare } from 'react-icons/fa';
import { useEffect, useRef, useState } from "react";
import tetrisSourceCode from "./examples/tetris";
import triangleSourceCode from "./examples/triangle";
import helloSourceCode from "./examples/hello";
import { CanvasDisplayer, Machine, TextAreaStatusWriter } from '@pagora/runner'
import { compile } from "@pagora/lang";

enum ExecutionState {
  STOPPED,
  COMPILING,
  RUNNING,
}

function App() {
  return <Page />
}

const initialSourceCode = `begin
  output("Hello, World!\\n");
end`

const machine = new Machine()

function Page() {
  const [code, setCode] = useState(initialSourceCode)
  const [execState, setExecState] = useState<ExecutionState>(ExecutionState.STOPPED)
  const statusRef = useRef<HTMLTextAreaElement>(null)
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const canvasWrapperRef = useRef<HTMLDivElement>(null)

  const start = () => {
    if (execState !== ExecutionState.STOPPED) return
    setExecState(ExecutionState.COMPILING)
    try {
      const program = compile(code)
      setExecState(ExecutionState.RUNNING)
      machine.start(program)
      canvasRef.current?.focus()
    } catch (e) {
      if (statusRef.current != null) {
        if (e instanceof Error) {
          statusRef.current.value = e.message
          console.log(e)
        }
      }
      setExecState(ExecutionState.STOPPED)
    }
  }

  const stop = () => {
    if (execState !== ExecutionState.RUNNING) return
    machine.stop()
    setExecState(ExecutionState.STOPPED)
  }

  useEffect(() => {
    if(statusRef.current !== null) {
      machine.attachStatusWriter(new TextAreaStatusWriter(statusRef.current))
    }
  }, [statusRef])

  useEffect(() => {
    if(canvasRef.current !== null && canvasWrapperRef.current !== null) {
      machine.attachDisplayer(new CanvasDisplayer(canvasRef.current))

      canvasRef.current.width = canvasWrapperRef.current.clientWidth
      canvasRef.current.height = canvasWrapperRef.current.clientHeight
      const resize = () => {
        if(canvasRef.current !== null && canvasWrapperRef.current !== null) {
          canvasRef.current.width = canvasWrapperRef.current.clientWidth
          canvasRef.current.height = canvasWrapperRef.current.clientHeight
        }
      }

      window.addEventListener('resize', resize)
      return () => { window.removeEventListener('resize', resize )}
    }
  }, [canvasRef])

  return (
    <div className="flex flex-col h-full">
      <NavBar onStart={start} onStop={stop} executionState={execState} setCode={setCode} />
      <div className="flex-1 flex overflow-hidden divide-x-2">
        <div className="flex-1 overflow-x-scroll">
          <ReactCodeMirror
            value={code}
            height="100%"
            maxHeight="100%"
            onChange={setCode}
            className="h-full"
          />
        </div>
        <div className="flex-1 flex flex-col divide-y-2">
          <div className="flex-1 overflow-clip" ref={canvasWrapperRef}>
            <canvas className="absolute border-0 focus:border-0" ref={canvasRef} tabIndex={1}/>
          </div>
          <textarea ref={statusRef} rows={7} disabled className="font-mono overflow-y-scroll px-3 py-1"></textarea>
        </div>
      </div>
      <Footer />
    </div>
  );
}

function NavBar(props: {
  setCode: (code: string) => void
  onStart: () => void,
  onStop: () => void,
  executionState: ExecutionState,
}) {
  const { setCode, onStart, onStop, executionState } = props
  return (
    <nav className="
    relative
    w-full
    flex
    flex-wrap
    items-center
    justify-between
    py-3
    bg-gray-700">
      <div className="w-full flex flex-wrap items-center px-6">
        <span className="text-xl text-white">Pagora</span>
        <div className="w-10"></div>
        <TemplateButton setCode={setCode} />
        <div className="flex-auto"></div>
        <ExecuteButton onStart={onStart} onStop={onStop} executionState={executionState} />
      </div>
    </nav>
  );
}

interface Template {
  name: string
  sourceCode: string
}

const templates: Template[] = [
  {
      name: "Hello",
      sourceCode: helloSourceCode,
  },
  {
      name: "Triangle",
      sourceCode: triangleSourceCode,
  },
  {
      name: "Tetris",
      sourceCode: tetrisSourceCode,
  },
]

function TemplateButton(props: {setCode: (code: string) => void}) {
  const {setCode} = props
  const [opened, setOpened] = useState(false)
  const [currentTemplate, setCurrentTemplate] = useState("")

  const menuRef = useRef<HTMLDivElement>(null)
  useEffect(() => {
    function handleClickOutside(event: any) {
      if (event.target === null) return
      if (menuRef.current === null) return
      if (menuRef.current.contains(event.target)) return
      setOpened(false)
    }
    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, [menuRef]);

  return <div ref={menuRef} className="w-40">
    <div 
      className="rounded border-solid text-white border-white border px-5 py-1 hover:bg-black hover:bg-opacity-20 hover:border-opacity-50 hover:cursor-pointer"
      onClick={() => {setOpened(!opened)}}>
      <div className="flex justify-between items-center">
        { currentTemplate || "Templates" }
        <FaCaretDown />
      </div>
    </div>
    { opened && <div className="absolute w-40 bg-white text-base z-50 float-left py-2 list-none text-left rounded-lg shadow-lg mt-1 m-0 bg-clip-padding border-none">
      {templates.map(({name, sourceCode}) => (
        <li key={name} onClick={() => {
          setCurrentTemplate(name)
          setOpened(false)
          setCode(sourceCode)
          }}>
          <span className="text-sm py-2 px-4 font-normal block w-full whitespace-nowrap bg-transparent text-gray-700 hover:bg-gray-100 hover:cursor-pointer">
            {name}
          </span>
        </li>
      ))}
    </div>}
  </div>
}

function ExecuteButton(props: {
  onStart: () => void,
  onStop: () => void,
  executionState: ExecutionState
}) {
  const {onStart, onStop, executionState} = props
  if (executionState === ExecutionState.STOPPED)
    return (<div onClick={onStart} className="rounded text-white bg-green-700 px-5 py-1 hover:bg-green-800 hover:border-opacity-50 hover:cursor-pointer">
      <div className="flex items-center">
        Run
        <div className="w-2"></div>
        <FaCaretRight />
      </div>
    </div>)
  else if (executionState === ExecutionState.COMPILING)
    return (<div className="rounded text-gray-300 bg-green-700 bg-opacity-80 px-5 py-1 hover:cursor-pointer">
      <div className="flex items-center">
        Compiling
        <div className="w-2"></div>
        <FaClock />
      </div>
    </div>)
  else
    return (<div onClick={onStop} className="rounded text-white bg-red-700 px-5 py-1 hover:bg-red-800 hover:border-opacity-50 hover:cursor-pointer">
      <div className="flex items-center">
        Stop
        <div className="w-2"></div>
        <FaSquare />
      </div>
    </div>)
}

function Footer() {
  return (
    <nav className="relative w-full flex flex-wrap items-center justify-between py-2 bg-gray-700 text-gray-100 hover:text-gray-700 focus:text-gray-700">
      <div className="w-full text-white text-center">
        <a href="https://github.com/jauhararifin/pagora">Github</a>
      </div>
    </nav>
  );
}

export default App;
