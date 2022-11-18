import ReactCodeMirror from "@uiw/react-codemirror";
import { FaCaretDown, FaCaretRight, FaClock, FaSquare } from 'react-icons/fa';
import { useState } from "react";

enum ExecutionState {
  STOPPED,
  COMPILING,
  RUNNING,
}

function App() {
  const [code, setCode] = useState("")
  return (
    <div className="flex flex-col h-full">
      <NavBar />
      <div className="flex-1 flex overflow-hidden divide-x-2">
        <div className="flex-1">
          <ReactCodeMirror
            value={code}
            height="100%"
            maxHeight="100%"
            onChange={setCode}
            className="h-full"
          />
        </div>
        <div className="flex-1 flex flex-col divide-y-2">
          <div className="flex-1">
            <canvas tabIndex={1}/>
          </div>
          <textarea rows={7} disabled className="font-mono overflow-y-scroll px-3 py-1">
            Status
          </textarea>
        </div>
      </div>
      <Footer />
    </div>
  );
}

function NavBar() {
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
        <NavButton />
        <div className="flex-auto"></div>
        <ExecuteButton executionState={ExecutionState.RUNNING} />
      </div>
    </nav>
  );
}

function NavButton() {
  return <div className="rounded
  border-solid
  text-white
  border-white
  border
  px-5
  py-1
  hover:bg-black
  hover:bg-opacity-20
  hover:border-opacity-50
  hover:cursor-pointer">
    <div className="flex items-center">
      Templates 
      <div className="w-3"></div>
      <FaCaretDown />
    </div>
  </div>
}

function ExecuteButton(props: {executionState: ExecutionState}) {
  const {executionState} = props
  if (executionState === ExecutionState.STOPPED)
    return (<div className="rounded
    text-white
    bg-green-700
    px-5
    py-1
    hover:bg-green-800
    hover:border-opacity-50
    hover:cursor-pointer">
      <div className="flex items-center">
        Run
        <div className="w-2"></div>
        <FaCaretRight />
      </div>
    </div>)
  else if (executionState === ExecutionState.COMPILING)
    return (<div className="rounded
    text-gray-300
    bg-green-700
    bg-opacity-80
    px-5
    py-1
    hover:cursor-pointer">
      <div className="flex items-center">
        Compiling
        <div className="w-2"></div>
        <FaClock />
      </div>
    </div>)
  else
    return (<div className="rounded
    text-white
    bg-red-700
    px-5
    py-1
    hover:bg-red-800
    hover:border-opacity-50
    hover:cursor-pointer">
      <div className="flex items-center">
        Stop
        <div className="w-2"></div>
        <FaSquare />
      </div>
    </div>)
}

function Footer() {
  return (
    <nav className="
    relative
    w-full
    flex
    flex-wrap
    items-center
    justify-between
    py-2
    bg-gray-700
    text-gray-100
    hover:text-gray-700
    focus:text-gray-700">
      <div className="w-full text-white text-center">
        <a href="https://github.com/jauhararifin/pagora">Github</a>
      </div>
    </nav>
  );
}

export default App;
