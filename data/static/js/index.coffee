do ->
	$  = (args...) -> document.querySelector    args...
	$$ = (args...) -> document.querySelectorAll args...
	c$ = (args...) -> document.createElement    args...

	request = (url, params, cb) ->
		unless cb
			cb = params
			params = {}

		firstParam = true
		for name, field of params
			if firstParam
				url += "?#{name}=#{encodeURIComponent field}"
				firstParam = false
			else
				url += "&#{name}=#{encodeURIComponent field}"

		req = new XMLHttpRequest
		req.onload = ->
			cb req.response

		req.open "GET", url
		req.send()

	$posts = $ "#posts"

	render = (posts) ->
		for post in posts
			$p = c$ "div"
			$p.className = "post"
			for content in post.content
				if content.tag is "Text"
					$c = c$ "p"
					$c.innerText = content.text
				else if content.tag is "ImageFull"
					$c = c$ "img"
					$c.src = "/file/" + content.filenames[content.filenames.length - 1].filename

				$p.appendChild $c

			$posts.appendChild $p

	request "/getFeed", {count: 1, offset: 0}, (resp) ->
		console.log resp
		render JSON.parse resp
