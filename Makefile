build:
	gulp

posts:
	gulp posts

deploy:
	rm -f dist.tar.gz
	tar -czf dist.tar.gz dist
	scp dist.tar.gz coot.me:/var/www/homepage
	ssh coot.me "cd /var/www/homepage && rm -rf dist && tar -xaf dist.tar.gz && rm dist.tar.gz"
	rm dist.tar.gz

.PHONY: deploy, build, posts
