import gmail
import json
import chardet

username = '<email address>'
password = '<password>'

g = gmail.login(username, password)

#retrieve from the sent box
sent = g.sent_mail().mail()
emails = {}

skip = 0 #skip indicator to skip a message due to retrieval error
for i in xrange(len(sent)):
	print '{0} of {1}'.format(i, len(sent))
	s = sent[i]
	
	try:
		s.fetch()
	except TypeError:
		skip = 1
	
	if skip == 0 and s != None:
		sub = s.subject
		body = s.body
		dest = s.to
		ts = str(s.sent_at)
	
		if body != None:
            #detect coding scheme to encode later in 'utf-8'
			char_body = chardet.detect(body)
			if char_body['encoding'] != None:
				try:
					body = body.decode(char_body['encoding']).encode('utf-8')
				except UnicodeDecodeError:
					body = ''
		else:
			body = ''
	
		emails[i] = {'sub': sub, 'body': body, 'dest': dest, 'ts': ts}

	skip = 0
	
with open('sent.json', 'w') as f:
	f.write(json.dumps(emails))