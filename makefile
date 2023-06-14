.PHONY: dev
dev:
	dotnet run --project Compiler $(path)

.PHONY: build
build:
	dotnet build
