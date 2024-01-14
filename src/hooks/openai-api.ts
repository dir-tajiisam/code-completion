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
  apiKey: string,
  maxTokens: number
): Promise<string> => {
  const requestData: ChatRequestData = {
    model: "gpt-4-1106-preview",
    // model: "gpt-3.5-turbo",
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
      Authorization: `Bearer ${apiKey}`,
    },
  };

  const res = await axios.post<ChatResponseData>(endpoint, requestData, config);
  const { message } = res.data.choices[0];
  return message.content;
};

const createConversionPrompt = (props: CodeConvertType) => {
  return `Please rewrite the following program in ${props.mode} and output it in a code block.

\`\`\`
${props.code}
\`\`\`
`;
};

const createExplainPrompt = (props: CodeConvertType) => {
  return `Please explain the following program in ${props.mode}.

\`\`\`
${props.code}
\`\`\`
`;
};

const createUnitTestPrompt = (props: CodeConvertType) => {
  return `Please write test code for the following program and output it in a code block. And explain the test code in ${props.mode}.

\`\`\`
${props.code}
\`\`\`
`;
};

const createRefactorPrompt = (props: CodeConvertType) => {
  return `Please refactor the code below to improve maintainability, testability, and reusability. Also, if it is written in the old style, please change it to more modern wording.
  Code should be described as code blocks, with explanations written in ${props.mode}.

\`\`\`
${props.code}
\`\`\`
`;
};

export type CodeConvertType = {
  code: string;
  mode: string;
  apiKey: string;
};

export const useConversionApi = () => {
  const [response, setResponse] = useState<{ code: string; mode: string }>({
    code: "",
    mode: "",
  });
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<AxiosError | null>(null);
  const [valid, setValid] = useState<boolean>(true);

  const handleConversionSubmit = async (props: CodeConvertType) => {
    setValid(true);
    setError(null);

    // call openai api
    setLoading(true);
    try {
      const prompt = createConversionPrompt(props);
      const response = await chatCompletion(prompt, props.apiKey, 2000);
      console.log(response);
      // abstract inner of code block from response
      try {
        const codeBlock = response.match(/```(\w*)\n([\s\S]*)```/);
        if (!codeBlock) {
          throw new Error("invalid response");
        }
        const mode = (codeBlock && codeBlock[1]) ?? "";
        const code = (codeBlock && codeBlock[2]) ?? "error";
        setResponse({ code, mode });
        setLoading(false);
      } catch (error) {
        const mode = "javascript";
        const code = `maybe error...\n${response}`;
        setResponse({ code, mode });
        setLoading(false);
      }
    } catch (error) {
      const axiosError = error as AxiosError;
      setError(axiosError);
      setLoading(false);
    }
  };

  return {
    response,
    loading,
    error,
    valid,
    handleSubmit: handleConversionSubmit,
  };
};

export const useExplainApi = () => {
  const [response, setResponse] = useState<{ code: string; mode: string }>({
    code: "",
    mode: "",
  });
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<AxiosError | null>(null);
  const [valid, setValid] = useState<boolean>(true);

  const handleSubmit = async (props: CodeConvertType) => {
    setValid(true);
    setError(null);

    // call openai api
    setLoading(true);
    try {
      const prompt = createExplainPrompt(props);
      const response = await chatCompletion(prompt, props.apiKey, 2000);
      // const response =
      //   '```java\npublic class OrderDiscount {\n    public static void main(String[] args) {\n        String companyName = "Semantic Designs";\n        String street = "8101 Asmara Dr.";\n        String city = "Austin";\n        String state = "TX";\n        int zip = 78750;\n\n        String item = "Blue widget";\n        int quantity = 217;\n        float price = 24.95f;\n\n        float totalAmount = 0;\n        final float discountThreshold = 1111.11f;\n        final int discountPercent = 20;\n        float discountAmount = 0;\n\n        totalAmount = quantity * price;\n        if (totalAmount > discountThreshold) {\n            discountAmount = (totalAmount * discountPercent) / 100f;\n            totalAmount = totalAmount - discountAmount;\n        }\n\n        System.out.println(companyName);\n        System.out.printf("Total: $%.2f", totalAmount);\n    }\n}\n```';
      console.log(response);
      // abstract inner of code block from response
      try {
        // const codeBlock = response.match(/```(\w*)\n([\s\S]*)```/);
        // if (!codeBlock) {
        //   throw new Error("invalid response");
        // }
        // const code = (codeBlock && codeBlock[2]) ?? "error";
        const code = response;
        setResponse({ code, mode: "markdown" });
        setLoading(false);
      } catch (error) {
        const mode = "markdown";
        const code = `maybe error...\n${response}`;
        setResponse({ code, mode: "markdown" });
        setLoading(false);
      }
    } catch (error) {
      const axiosError = error as AxiosError;
      setError(axiosError);
      setLoading(false);
    }
  };

  return {
    response,
    loading,
    error,
    valid,
    handleSubmit: handleSubmit,
  };
};

export const useUnitTestApi = () => {
  const [response, setResponse] = useState<{ code: string; mode: string }>({
    code: "",
    mode: "",
  });
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<AxiosError | null>(null);
  const [valid, setValid] = useState<boolean>(true);

  const handleSubmit = async (props: CodeConvertType) => {
    setValid(true);
    setError(null);

    // call openai api
    setLoading(true);
    try {
      const prompt = createUnitTestPrompt(props);
      const response = await chatCompletion(prompt, props.apiKey, 2000);
      // abstract inner of code block from response
      try {
        // const codeBlock = response.match(/```(\w*)\n([\s\S]*)```/);
        // if (!codeBlock) {
        //   throw new Error("invalid response");
        // }
        // const mode = (codeBlock && codeBlock[1]) ?? "";
        // const code = (codeBlock && codeBlock[2]) ?? "error";
        const code = response;
        const mode = "markdown";
        setResponse({ code, mode });
        setLoading(false);
      } catch (error) {
        const mode = "markdown";
        const code = `maybe error...\n${response}`;
        setResponse({ code, mode });
        setLoading(false);
      }
    } catch (error) {
      const axiosError = error as AxiosError;
      setError(axiosError);
      setLoading(false);
    }
  };

  return {
    response,
    loading,
    error,
    valid,
    handleSubmit: handleSubmit,
  };
};

export const useRefactorApi = () => {
  const [response, setResponse] = useState<{ code: string; mode: string }>({
    code: "",
    mode: "",
  });
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<AxiosError | null>(null);
  const [valid, setValid] = useState<boolean>(true);

  const handleSubmit = async (props: CodeConvertType) => {
    setValid(true);
    setError(null);

    // call openai api
    setLoading(true);
    try {
      const prompt = createRefactorPrompt(props);
      const response = await chatCompletion(prompt, props.apiKey, 2000);
      // abstract inner of code block from response
      try {
        // const codeBlock = response.match(/```(\w*)\n([\s\S]*)```/);
        // if (!codeBlock) {
        //   throw new Error("invalid response");
        // }
        // const mode = (codeBlock && codeBlock[1]) ?? "";
        // const code = (codeBlock && codeBlock[2]) ?? "error";
        const code = response;
        const mode = "markdown";
        setResponse({ code, mode });
        setLoading(false);
      } catch (error) {
        const mode = "markdown";
        const code = `maybe error...\n${response}`;
        setResponse({ code, mode });
        setLoading(false);
      }
    } catch (error) {
      const axiosError = error as AxiosError;
      setError(axiosError);
      setLoading(false);
    }
  };

  return {
    response,
    loading,
    error,
    valid,
    handleSubmit: handleSubmit,
  };
};
