build:
	gulp

posts:
	gulp posts

deploy:
	rm -f dist.tar.gz
	tar -czf dist.tar.gz dist
	scp dist.tar.gz homepage:/var/www/homepage
	ssh homepage "cd /var/www/homepage && rm -rf dist && tar -xaf dist.tar.gz && rm dist.tar.gz"
	rm dist.tar.gz

.PHONY: deploy, build, posts
