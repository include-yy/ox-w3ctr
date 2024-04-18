(function() {
   'use strict'
    window.addEventListener('load', () => {
	let tocToggle = document.getElementById('toc-toggle')
	let arrow = tocToggle.children[0]
	let text = tocToggle.children[1]
	let myclass = document.body.classList

	let fontSize = window.getComputedStyle(document.documentElement).fontSize
	let pixWidth = parseFloat(fontSize)
	let widthEm = window.innerWidth / pixWidth
	let tocCount = document.getElementById('toc').dataset.count
	if (widthEm > 80 && tocCount >= 5) {
	    myclass.remove('toc-inline')
	    myclass.add('toc-sidebar')
	    arrow.innerHTML = '←'
	    text.innerHTML = 'Collapse Sidebar'
	}
	tocToggle.addEventListener('click', () => {
	    if (myclass.contains('toc-inline')) {
		myclass.remove('toc-inline')
		myclass.add('toc-sidebar')
		arrow.innerHTML = '←'
		text.innerHTML = 'Collapse Sidebar'
	    } else if (myclass.contains('toc-sidebar')) {
		myclass.remove('toc-sidebar')
		myclass.add('toc-inline')
		arrow.innerHTML = '→'
		text.innerHTML = 'Pop Out Sidebar'
	    }
	})
    })
})()
