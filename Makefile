.PHONY: frontend backend
frontend:
	nix-build -A ghcjs.reflex-skeleton-frontend -o result-frontend

backend:
	nix-build -A ghc.reflex-skeleton-backend -o result-backend

release: frontend backend
	mkdir -p dist
	cp -f result-frontend/bin/reflex-skeleton.jsexe/{index.html,lib.js,rts.js,out.js,runmain.js} ./dist/
	cp -f result-backend/bin/reflex-skeleton-backend ./dist
