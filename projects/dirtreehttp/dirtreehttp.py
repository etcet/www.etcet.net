#!/usr/bin/python
# dirtreehttp: scrape directory tree by crawling urls
# Chris James - etcet.net
# copyleft - all wrongs reversed

#for crawler
import urllib2
import re
import os
from xml.dom.ext import PrettyPrint
from BeautifulSoup import *
from urlparse import urljoin
from xml.dom.minidom import Document
#for main
import getopt, sys

class crawler:
	urls = set()
	newpages = set()
	doc = None
	root = None
	tocrawl = [ ['a','href'], ['link','href'], ['script','src'], ['img','src'], ['iframe', 'src'], ['form','action'], ['source', 'src'], ['embed', 'src'] ]

	def __init__(self):
		self.doc = Document()
		self.root = self.doc.createElement('root')
		self.doc.appendChild(self.root)
	
	def addtoindex(self, url):
		#add to indexed url set
		protocol = 'http://'

		if url[:5] == 'https':
			protocol = url[:8]
			url = url[8:]
		else:
			protocol = url[:7]
			url = url[7:]

		urlparts = []
		params = url.split('?',1)
		if len(params) > 1:
			url = params[0]
			urlparts = url.split('/')
			urlparts[-1] += '?' + params[1]
		else:
			urlparts = url.split('/')

		urlparts[0] = protocol + urlparts[0]

		#create new path in tree for url
		self.addtonode(self.root, urlparts)
	
	def isindexed(self, url):
		return url in self.urls
	
	def crawl(self, pages, domain, depth=10):
		self.urls = set()

		for i in range(depth):
			print "Crawling at depth %s" % i 
			self.newpages = set()
			for page in pages:
				if self.isindexed(page): continue

				try:
					c = urllib2.urlopen(page)
					if not re.search('text/html', c.info()['Content-Type']):
						continue
					else:
						if c.geturl() != page:
							print "  Following redirect from {0} to {1}".format(page, c.geturl())
							page = c.geturl()
							if self.isindexed(page): continue
							self.urls.add(page)
						else:
							print "  Parsing %s" % page
				except:
					print "  Could not open %s" % page
					continue
				soup = BeautifulSoup(c.read())

				for tag,link in self.tocrawl:
					elements = soup.findAll(tag)
					for element in elements:
						if (link in dict(element.attrs)):
							url = urljoin(page, element[link])
							if url.find("'") != -1: continue
							url = url.split('#')[0]
							if url[0:4] == 'http' and re.search(domain,url) and not self.isindexed(url):
								self.addtoindex(url)

			if pages == self.newpages:
				return
			else:
				pages = self.newpages

	def ls(self):
		for url in sorted(self.urls):
			print url
			
	def toxml(self):

		for url in sorted(self.urls):
			urlparts = url[7:].split('/')
			self.addtonode(self.root, urlparts)

	def addtonode(self, node, urlparts):
		if urlparts and not urlparts == ['']:
			#does node already exist?
			newnode = None
			for child in node.childNodes:
				if child.getAttribute('id') == urlparts[0]:
					newnode = child
			#if not, create node and add it to 
			if not newnode:
				newnode = self.doc.createElement('node')
				node.appendChild(newnode)
				newnode.setAttribute('id', urlparts[0].encode("utf-8") )
				newnode.setIdAttribute('id')
				#notice: we are touching self.newpages which effects the main crawl loop
				#this effectively indexes all directories
				url = self.geturl(newnode)
				self.newpages.add(url)

			urlparts = urlparts[1:]
			self.addtonode(newnode, urlparts)
	
	def geturl(self, node):
		if node.parentNode.tagName == 'root':
			return node.getAttribute('id')
		if node.parentNode and node.tagName != 'root':
			return str(self.geturl(node.parentNode)) + '/' + node.getAttribute('id')


	def printtree(self, node, tab=0):
		for child in node.childNodes:
			if tab==0: print
			print "|   "*tab+child.getAttribute('id')
			self.printtree(child, tab+1)
		
	def printhtml(self, node, filename='out.html'):
		f = open(filename, 'w')
		f.write('<html><head><title></title></head><body><pre>')
		self.printhtmlnode(node, f)
		f.write('</pre></body></html>')
		print "\nHTML version at file://{0}/{1}".format(os.getcwd(), filename)

	def printhtmlnode(self, node, f, tab=0):
		for child in node.childNodes:	
			if tab==0: f.write('\n')
			f.write( '|   '*tab+'<a href="'+self.geturl(child)+'">'+child.getAttribute('id')+'</a>\n')
			self.printhtmlnode(child, f, tab+1)

	def printxml(self):
		xml.dom.ext.PrettyPrint(self.doc)

def usage():
	print 'Usage dirtreehttp [OPTION]... [URL]...'
	print 'Create directory tree for URL by scraping links.'
	print 'EXAMPLE: dirtreehttp -d 1 -r google.com http://www.google.com/'
	print '\nArguments (optional but highly recommended):'
	print '\t-d depth\t--depth depth\t\tRecrusively crawl urls depth times (default: 2)'
	print '\t-r string\t--restrict string\tOnly scrape urls that contain string, i.e. re.search(string,url)'
	print '\t-o FILE\t\t--output FILE\t\tWrite html tree to FILE'
	print '\t-h\t\t--help\t\t\tShow this message'

	sys.exit()

def main():
	if len(sys.argv) < 2: usage()
	
	try:
		opts, args = getopt.getopt(sys.argv[1:], "hr:d:vo:", ["help", "restrict=", "depth=", "version", "output="])
	except getopt.GetoptError, err:
		# print help information and exit:
		print str(err) # will print something like "option -a not recognized"
		usage()

	domains = []
	re_search = '\w'
	depth = 2
	filename = 'out.html'
	for o, a in opts:
		if o in ("-h", "--help"):
			usage()
		elif o in ("-r", "--restrict"):
			re_search = a
		elif o in ("-d", "--depth"):
			depth = int(a)
		elif o in("-o", "--output"):
			filename = a
		elif a:
			print a
			domains = [ a ]
		else:
			assert False, "unhandled option"

	for a in args:
		print a
		domains.append(a)
    
	c = crawler()
	print depth
	c.crawl(domains, re_search, depth)
	c.printtree(c.root)
	c.printhtml(c.root, filename)
	#crawler.printxml()

if __name__ == "__main__":
	main()
