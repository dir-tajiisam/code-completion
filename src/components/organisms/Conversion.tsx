import React, { FC, useEffect, useRef, useState } from "react";
import CodeCompletion from "../molecules/CodeCompletion";
import { useConversionApi } from "@/hooks/openai-api";
import { AxiosError } from "axios";

export type ConversionProps = {
  init: string;
  apiKey: string;
  setLoading: (loading: boolean) => void;
  setError: (error: AxiosError) => void;
};

const Conversion: FC<ConversionProps> = (props) => {
  const { init, apiKey, setLoading, setError } = props;

  const [before, setBefore] = useState<string>(init);
  // const [after, setAfter] = useState<string>("\n".repeat(29));
  const [mode, setMode] = useState<string>("");
  const [canCallApi, setCanCallApi] = useState<boolean>(false);

  // call api
  const { response, loading, error, valid, handleSubmit } = useConversionApi();
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
        { value: "python", label: "Python" },
        // { value: "javascript", label: "JavaScript" },
        { value: "java", label: "Java" },
        // { value: "csharp", label: "C#" },
        // { value: "cpp", label: "C++" },
        // { value: "go", label: "Go" },
        { value: "cobol", label: "COBOL" },
      ]}
      canCallApi={canCallApi}
      setBefore={setBefore}
      setMode={setMode}
      handleSubmit={onClick}
    />
  );
};

export default Conversion;
