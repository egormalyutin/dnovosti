do ->
	$  = (args...) -> document.querySelector    args...
	$$ = (args...) -> document.querySelectorAll args...

	postForm = $ "#post-form"

	postContent = $ '#post-content'

	addText  = $ '#add-text'
	addImage = $ '#add-image'

	postName = $ "#post-name"

	addText.onclick = ->
		elem = document.createElement "input"
		elem.type = "text"
		postContent.appendChild elem

	addImage.onclick = ->
		elem = document.createElement "input"
		elem.type = "file"
		postContent.appendChild elem

	postForm.onsubmit = ->
		n = 0
		for child in [postContent.children...]
			if child.type is "text"
				child.name = "text" + n
			else if child.type is "file"
				child.name = "image" + n
			n++
		postName.name = "name"
		return true
