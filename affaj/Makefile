ide:
	ghcid --command "stack ghci --ghci-options=-fno-code --main-is affaj:lib affaj:exe:affaj affaj:test:affaj-test"

postgres:
	docker run -p 5432\:5432 -e POSTGRES_USER=root -e POSTGRES_DB=affaj postgres\:10.5-alpine

sql-repl:
	psql -h localhost -p 5432 -U root -d affaj
