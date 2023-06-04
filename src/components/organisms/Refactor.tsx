import React, { FC, useEffect, useRef, useState } from "react";
import CodeCompletion from "../molecules/CodeCompletion";
import { useRefactorApi } from "@/hooks/openai-api";
import { AxiosError } from "axios";

export type RefactorProps = {
  init: string;
  apiKey: string;
  setLoading: (loading: boolean) => void;
  setError: (error: AxiosError) => void;
};

const Refactor: FC<RefactorProps> = (props) => {
  const { init, apiKey, setLoading, setError } = props;

  const [before, setBefore] = useState<string>(init);
  const [mode, setMode] = useState<string>("");
  const [canCallApi, setCanCallApi] = useState<boolean>(false);

  // call api
  const { response, loading, error, valid, handleSubmit } = useRefactorApi();
  const onClick = async () => {
    handleSubmit({ code: before, apiKey: apiKey, mode: mode });
  };

  useEffect(() => {
    setCanCallApi(before !== "" && mode !== "" && apiKey !== "");
  }, [before, mode, apiKey]);

  useEffect(() => {
    setLoading(loading);
  }, [setLoading, loading]);

  useEffect(() => {
    if (error === null) return;
    setError(error);
  }, [setError, error]);

  return (
    <CodeCompletion
      before={before}
      after={response.code}
      mode={response.mode || mode}
      modeList={[
        { value: "japanese", label: "日本語" },
        // { value: "javascript", label: "JavaScript" },
        { value: "english", label: "英語" },
        // { value: "csharp", label: "C#" },
        // { value: "cpp", label: "C++" },
        // { value: "go", label: "Go" },
        { value: "chinese", label: "中国語" },
      ]}
      canCallApi={canCallApi}
      setBefore={setBefore}
      setMode={setMode}
      handleSubmit={onClick}
      buttonTitle="Refactor!!"
    />
  );
};

export default Refactor;
