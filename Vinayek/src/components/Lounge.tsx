import { useEffect, useMemo, useRef, useState } from "react";
import {
  MeetingProvider,
  MeetingConsumer,
  useMeeting,
  useParticipant,
} from "@videosdk.live/react-sdk";
import { authToken, createMeeting } from "../server/videosdk";
import ReactPlayer from "react-player";
import { useWallet } from "@meshsdk/react";
import { IoMdMic, IoMdMicOff } from "react-icons/io";
import { FaVideo, FaVideoSlash } from "react-icons/fa";

export function ParticipantView(props: { participantId: string }) {
  const micRef = useRef<HTMLAudioElement>(null);
  const { webcamStream, micStream, webcamOn, micOn, isLocal, displayName } =
    useParticipant(props.participantId);

  const videoStream = useMemo(() => {
    if (webcamOn && webcamStream) {
      const mediaStream = new MediaStream();
      mediaStream.addTrack(webcamStream.track);
      return mediaStream;
    }
  }, [webcamStream, webcamOn]);

  useEffect(() => {
    if (micRef.current) {
      if (micOn && micStream) {
        const mediaStream = new MediaStream();
        mediaStream.addTrack(micStream.track);

        micRef.current.srcObject = mediaStream;
        micRef.current
          .play()
          .catch((error) =>
            console.error("videoElem.current.play() failed", error)
          );
      } else {
        micRef.current.srcObject = null;
      }
    }
  }, [micStream, micOn]);

  return (
    <div
      className="w-[400px] h-[300px] p-4 bg-[#4f4f4f] rounded-[10px] relative"
      key={props.participantId}
    >
      <audio ref={micRef} autoPlay muted={isLocal} />
      {webcamOn && (
        <ReactPlayer
          playsinline
          pip={false}
          light={false}
          controls={false}
          muted={true}
          playing={true}
          url={videoStream}
          height={"100%"}
          width={"100%"}
          onError={(err) => {
            console.log(err, "participant video error");
          }}
          style={{
            borderRadius: "10px",
            overflow: "hidden",
          }}
        />
      )}
      <div className="absolute bottom-0 right-0 p-4 flex gap-x-4 items-center opacity-75">
        <div className="text-sm">{displayName.slice(0, 18)}...</div>
        <div className="">
          {webcamOn ? <FaVideo size={14} /> : <FaVideoSlash size={14} />}
        </div>
        <div className="">
          {micOn ? <IoMdMic size={14} /> : <IoMdMicOff size={14} />}
        </div>
      </div>
    </div>
  );
}

export function Controls() {
  const { leave, toggleMic, toggleWebcam, localMicOn, localWebcamOn } =
    useMeeting();
  return (
    <div>
      <div className="flex gap-x-4 items-center justify-center mt-4">
        <div
          onClick={() => toggleWebcam()}
          className="rounded-full border px-6 py-2 cursor-pointer"
        >
          {localWebcamOn ? <FaVideo size={22} /> : <FaVideoSlash size={22} />}
        </div>
        <div
          onClick={() => toggleMic()}
          className="rounded-full border px-6 py-2 cursor-pointer"
        >
          {localMicOn ? <IoMdMic size={22} /> : <IoMdMicOff size={22} />}
        </div>
        <div
          onClick={() => leave()}
          className="rounded-full border px-6 py-2 cursor-pointer"
        >
          Leave
        </div>
      </div>
    </div>
  );
}

export function MeetingView(props: {
  meetingId: string;
  onMeetingLeave: () => void;
  joined: "JOINED" | "JOINING" | null;
}) {
  const [joined, setJoined] = useState<"JOINED" | "JOINING" | null>(null);
  const { join } = useMeeting();
  const { participants } = useMeeting({
    onMeetingJoined: () => {
      setJoined("JOINED");
    },
    onMeetingLeft: () => {
      props.onMeetingLeave();
    },
  });

  const joinMeeting = () => {
    setJoined("JOINING");
    join();
  };

  useEffect(() => {
    joinMeeting();
  }, []);

  return (
    <div className="rounded-lg p-8 border flex flex-col items-center">
      <div className="text-xl">Lounge</div>
      <div>
        {joined && joined == "JOINING" && (
          <div className="h-[300px] flex justify-center items-center my-4">
            Joining the meeting...
          </div>
        )}
      </div>
      {joined && joined == "JOINED" && (
        <div className="">
          <div className="flex flex-wrap gap-4 justify-around py-10">
            {[...participants.keys()].map((participantId) => (
              <ParticipantView
                participantId={participantId}
                key={participantId}
              />
            ))}
          </div>
          <Controls />
        </div>
      )}
    </div>
  );
}

export default function Meeting({ meetingId }: { meetingId: string }) {
  const [address, setAddress] = useState<string | null>(null);

  const { wallet, connected } = useWallet();

  const [videoOn, setVideoOn] = useState(true);
  const [micOn, setMicOn] = useState(true);

  useEffect(() => {
    const loadStakekey = () => {
      (async () => {
        try {
          if (connected) {
            const [address] = await wallet.getUsedAddresses();
            setAddress(address);
          }
        } catch (error) {
          console.log(error);
        }
      })();
    };

    void loadStakekey();
  }, [address]);

  const videoRef = useRef<ReactPlayer>(null);

  const [videoStream, setVideoStream] = useState<MediaStream | undefined>();

  useEffect(() => {
    // Request access to the webcam
    const getVideo = async () => {
      try {
        const stream = await navigator.mediaDevices.getUserMedia({
          video: true,
        });

        setVideoStream(stream);
      } catch (err) {
        console.error("Error accessing the camera", err);
      }
    };

    if (videoOn) {
      getVideo();
    }

    if (!videoOn) {
      if (videoStream) {
        const tracks = videoStream.getTracks();

        // Stop each track
        tracks.forEach((track) => track.stop());

        setVideoStream(undefined);
      }
    }

    return () => {
      if (videoStream) {
        videoStream.getTracks().forEach((track) => track.stop());
      }
    };
  }, [videoOn]);

  const [joined, setJoined] = useState<"JOINED" | "JOINING" | null>(null);

  return authToken && joined ? (
    <MeetingProvider
      config={{
        meetingId,
        micEnabled: micOn,
        webcamEnabled: videoOn,
        name: address ?? "Anon",
      }}
      token={authToken}
    >
      <MeetingConsumer>
        {() => (
          <MeetingView
            joined={joined}
            meetingId={meetingId}
            onMeetingLeave={() => {
              setJoined(null);
            }}
          />
        )}
      </MeetingConsumer>
    </MeetingProvider>
  ) : (
    <div className="rounded-lg p-8 border flex flex-col items-center">
      <div className="text-xl">Lounge</div>
      <div className="h-[400px]">
        <div className="w-[400px] py-10 rounded-lg overflow-hidden">
          {videoStream ? (
            <ReactPlayer
              //
              playsinline // very very imp prop
              pip={false}
              light={false}
              controls={false}
              muted={true}
              playing={true}
              //
              url={videoStream}
              ref={videoRef}
              //
              height={"100%"}
              width={"100%"}
              style={{
                borderRadius: "10px",
                overflow: "hidden",
              }}
              onError={(err) => {
                console.log(err, "participant video error");
              }}
            />
          ) : (
            <div className="w-full min-h-[300px] flex justify-center items-center bg-[#4f4f4f] rounded-[10px]">
              <FaVideoSlash className="opacity-60" size={28} />
            </div>
          )}
        </div>
      </div>
      <div className="flex gap-x-4 items-center justify-center">
        <div
          onClick={() => setVideoOn((prev) => !prev)}
          className="rounded-full border px-6 py-2 cursor-pointer"
        >
          {videoOn ? <FaVideo size={22} /> : <FaVideoSlash size={22} />}
        </div>
        <div
          onClick={() => setMicOn((prev) => !prev)}
          className="rounded-full border px-6 py-2 cursor-pointer"
        >
          {micOn ? <IoMdMic size={22} /> : <IoMdMicOff size={22} />}
        </div>
        <div
          onClick={() => setJoined("JOINING")}
          className="rounded-full border px-6 py-2 cursor-pointer"
        >
          Join
        </div>
      </div>
    </div>
  );
}
