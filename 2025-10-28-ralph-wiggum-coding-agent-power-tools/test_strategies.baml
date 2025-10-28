// Test file for client strategies (fallback and round-robin)

retry_policy MyRetryPolicy {
  max_retries 3
  strategy {
    type exponential_backoff
    delay_ms 200
    multiplier 1.5
    max_delay_ms 10000
  }
}

client<llm> ClientA {
  provider "openai"
  options {
    model "gpt-4"
    api_key env.OPENAI_API_KEY
  }
}

client<llm> ClientB {
  provider "anthropic"
  options {
    model "claude-sonnet-4"
    api_key env.ANTHROPIC_API_KEY
  }
}

client<llm> ClientC {
  provider "openai"
  options {
    model "gpt-3.5-turbo"
    api_key env.OPENAI_API_KEY
  }
}

// Fallback provider test
client<llm> ResilientClient {
  provider fallback
  retry_policy MyRetryPolicy
  options {
    strategy [
      ClientA
      ClientB
      ClientC
    ]
  }
}

// Round-robin provider test (using identifier with underscore instead of hyphen)
client<llm> LoadBalancedClient {
  provider round_robin
  options {
    strategy [
      ClientA
      ClientB
    ]
    start 0
  }
}
