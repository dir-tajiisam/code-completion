import axios, { AxiosError, AxiosRequestConfig } from "axios";
import { useState } from "react";

type ChatRequestData = {
  model: string;
  messages: { role: string; content: string }[];
  max_tokens: number;
};

type ChatResponseData = {
  usage: {
    prompt_tokens: number;
    completion_tokens: number;
    total_tokens: number;
  };
  choices: {
    message: {
      role: string;
      content: string;
    };
    finish_reason: string;
    index: number;
  }[];
};

const endpoint = "https://api.openai.com/v1/chat/completions";

const chatCompletion = async (
  prompt: string,
  key: string,
  maxTokens: number
): Promise<string> => {
  const requestData: ChatRequestData = {
    model: "gpt-3.5-turbo",
    // model: "gpt-4",
    messages: [
      {
        role: "system",
        content:
          "We're refactoring the system. Please lend me your assistance.",
      },
      { role: "user", content: prompt },
    ],
    max_tokens: maxTokens,
  };

  const config: AxiosRequestConfig = {
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${key}`,
    },
  };

  const res = await axios.post<ChatResponseData>(endpoint, requestData, config);
  const { message } = res.data.choices[0];
  return message.content;
};

const createPrompt = (props: CodeConvertType) => {
  return `Please rewrite the following program in ${props.mode} and output it in a code block.

  \`\`\`
${props.code}
\`\`\`
`;
};

export type CodeConvertType = {
  code: string;
  mode: string;
  key: string;
};

export const useApi = () => {
  const [response, setResponse] = useState<{ code: string; mode: string }>({
    code: "",
    mode: "",
  });
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>("");
  const [valid, setValid] = useState<boolean>(true);

  const handleSubmit = async (props: CodeConvertType) => {
    setValid(true);
    setError("");

    // call openai api
    setLoading(true);
    const prompt = createPrompt(props);
    const response = await chatCompletion(prompt, props.key, 400);
    // const response =
    //   '```java\npublic class OrderDiscount {\n    public static void main(String[] args) {\n        String companyName = "Semantic Designs";\n        String street = "8101 Asmara Dr.";\n        String city = "Austin";\n        String state = "TX";\n        int zip = 78750;\n\n        String item = "Blue widget";\n        int quantity = 217;\n        float price = 24.95f;\n\n        float totalAmount = 0;\n        final float discountThreshold = 1111.11f;\n        final int discountPercent = 20;\n        float discountAmount = 0;\n\n        totalAmount = quantity * price;\n        if (totalAmount > discountThreshold) {\n            discountAmount = (totalAmount * discountPercent) / 100f;\n            totalAmount = totalAmount - discountAmount;\n        }\n\n        System.out.println(companyName);\n        System.out.printf("Total: $%.2f", totalAmount);\n    }\n}\n```';
    console.log(response);
    // abstract inner of code block from response
    const codeBlock = response.match(/```(\w+)\n([\s\S]*)```/);
    const mode = (codeBlock && codeBlock[1]) ?? "error";
    const code = (codeBlock && codeBlock[2]) ?? "error";
    setResponse({ code, mode });
    setLoading(false);
  };

  return { response, loading, error, valid, handleSubmit };
};