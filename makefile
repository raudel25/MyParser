.PHONY: dev
dev:
	dotnet run --project Compiler $(file)

.PHONY: build
build:
	dotnet build
