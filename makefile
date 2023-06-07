.PHONY: dev
dev:
	dotnet run --project Execute $(file)

.PHONY: build
build:
	dotnet build
