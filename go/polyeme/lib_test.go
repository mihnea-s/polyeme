package polyeme

import (
	"testing"
)

func Test(t *testing.T) {
	if Hello() != "Hello, Polyeme!" {
		t.Fatalf("expected a hello")
	}
}
