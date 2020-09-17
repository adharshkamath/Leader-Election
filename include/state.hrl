-record(state, {
        name :: atom(),
        leader :: atom(),
        node :: atom(),
        nodes :: [atom()],
        timeout :: integer()
}).