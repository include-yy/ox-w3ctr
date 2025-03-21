import * as readline from 'readline'
import { stdin as input, stdout as output} from 'process'

import * as Mathjax from 'mathjax'
import { JSONRPCServer } from 'json-rpc-2.0'
import { Command } from 'commander'

const server = new JSONRPCServer()
const program = new Command()

program.option('--timeout <number>')

const procTimeout = (timeout) => {
    let count = 0
    let handle = null
    let reset = () => {
	handle && clearTimeout(handle)
	handle = setTimeout(() => {
	    count === 0 && process.exit(0)
	    count = 0
	    reset()
	}, timeout)
    }
    reset()
    return () => { count++ }
}

const main = () => {
    program.parse(process.argv)
    const options = program.opts()
    const timeout = options.timeout || 30000
    const incf = procTimeout(timeout)
    const rl = readline.createInterface({input, output})
    rl.on('line', (line) => {
	incf()
	server.receiveJSON(line).then((response) => {
	    if (response) {
		console.log(JSON.stringify(response));
	    }
	})
    })
}

const str2MathML = await (async () => {
    const Jax2ML = await Mathjax.init({
	loader: {load: ['input/tex', 'adaptors/liteDOM']}
    })
    return (str) => {
	const regex = /^\s*\\\(/
	const mre = /\\\((.*?)\\\)|\\\[(.*?)\\\]/g;
	const display = regex.test(str)
	const match = mre.exec(str)
	const text = (match != null) ? (match[1] || match[2]) : str
	const result = Jax2ML.tex2mml(text, {display: !display})
	return result
    }
})()

server.addMethod("echo", ({ text }) => text)
server.addMethod("add", ([a, b]) => a + b)
server.addMethod("tex2mml", str2MathML)

main()
