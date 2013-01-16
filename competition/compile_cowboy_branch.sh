REPO="$1"
BRANCH="$2"
DIR="cowboy-$BRANCH"


function checkout {
    git clone "$REPO" "$DIR"
    pushd "$DIR"
       git checkout "$BRANCH"
    popd
}

function compile {
    pushd "$DIR"
        ../../rebar get-deps compile
    popd
}

checkout
compile
