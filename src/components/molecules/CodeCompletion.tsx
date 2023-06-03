import React, { FC, useRef } from "react";
import { Button, HStack, Select, VStack } from "@chakra-ui/react";
import dynamic from "next/dynamic";
import { randomUUID } from "crypto";

const CodeEditor = dynamic(
  () => import("@/components/molecules/editor/CodeEditor"),
  { ssr: false }
);

export type CodeCompletionProps = {
  before: string;
  after: string;
  mode: string;
  modeList: { value: string; label: string }[];
  canCallApi: boolean;
  setBefore: (before: string) => void;
  setMode: (mode: string) => void;
  handleSubmit: () => void;
};

const CodeCompletion: FC<CodeCompletionProps> = (props) => {
  const {
    before,
    after,
    mode,
    modeList,
    canCallApi,
    setBefore,
    setMode,
    handleSubmit,
  } = props;

  // call api
  const onClick = async () => {
    handleSubmit();
  };

  return (
    <HStack justifyContent={"center"}>
      <CodeEditor
        value={before}
        onChange={(value, event) => setBefore(value)}
        width={"38vw"}
        maxLines={40}
        mode="javascript"
      />
      <VStack width={"10vw"}>
        <Button
          ml="3vw"
          mr="3vw"
          type="button"
          onClick={() => onClick()}
          shadow={"lg"}
          width={"100%"}
          bgColor={"pink.200"}
          isDisabled={!canCallApi}
        >
          {"Conversion!!"}
        </Button>
        <Select
          textAlign={"center"}
          variant={"outline"}
          placeholder="Language"
          shadow={"lg"}
          bgColor={"pink.200"}
          fontWeight={"bold"}
          onChange={(e) => setMode(e.target.value)}
        >
          {modeList.map((mode) => (
            <option key={mode.value} value={mode.value}>
              {mode.label}
            </option>
          ))}
        </Select>
      </VStack>
      <CodeEditor value={after} width={"38vw"} maxLines={40} mode={mode} />
    </HStack>
  );
};

export async function getStaticProps() {
  return {
    props: {},
  };
}

export default CodeCompletion;
